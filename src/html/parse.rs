/**
 * [html specs](https://html.spec.whatwg.org/multipage/)
 */
use crate::html::dom::{ElementData, Node as DomNode, NodeType as DomNodeType};
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_till, take_until, take_while_m_n};
use nom::bytes::streaming::{tag_no_case, take_till1};
use nom::character::complete::{alphanumeric1, multispace0, multispace1};
use nom::combinator::{fail, map, opt, peek};
use nom::error::{context, ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, separated_pair, tuple};
use nom::{AsChar, Err as NomErr, IResult};
use std::collections::{HashMap, VecDeque};
use tracing;

pub type ParseHTMLRes<T, U> = IResult<T, U, VerboseError<T>>;

pub static EMPTY_STR: & str = "";

#[derive(Debug, PartialEq, Eq)]
pub enum ParseHTMLSnippet<'a> {
  OpeningStartTag {
    tag_name: &'a str,
    attributes: HashMap<&'a str, &'a str>,
  },
  SelfClosingStartTag {
    tag_name: &'a str,
    attributes: HashMap<&'a str, &'a str>,
  },
  EndTag {
    tag_name: &'a str,
  },
  Text {
    text: &'a str,
  },
  DocTypeTag {
    legacy_compact: bool,
  },
  CommentTag {
    text: &'a str,
  },
  CDataTag {
    text: &'a str,
  },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseHTMLNode {
  Closing(DomNode),
  Opening(DomNode),
}

pub fn parse_elem_attr_key(input: &str) -> ParseHTMLRes<&str, &str> {
  context(
    "elem attr key",
    take_till1(|c: char| c.is_control() || " \"'></=\t\n\r".contains(c)),
  )(input)
}

pub fn parse_elem_attr_value_content_without_quote(input: &str) -> ParseHTMLRes<&str, &str> {
  context(
    "elem attr value content without quote",
    take_till1(|c: char| c.is_control() || " \"'=<>`".contains(c) || c.is_whitespace()),
  )(input)
}

pub fn parse_basic_unicode_escapable(input: &str) -> ParseHTMLRes<&str, &str> {
  preceded(
    peek(tag("u")),
    take_while_m_n(4, 4, |c: char| c == 'u' || c.is_hex_digit()),
  )(input)
}

pub fn parse_entire_unicode_escapable(input: &str) -> ParseHTMLRes<&str, &str> {
  preceded(
    peek(tag("u")),
    delimited(
      peek(tag("{")),
      take_while_m_n(4, 9, |c: char| "u{}".contains(c) || c.is_hex_digit()),
      peek(tag("}")),
    ),
  )(input)
}

pub fn parse_string_escapable(input: &str) -> ParseHTMLRes<&str, &str> {
  alt((
    take_while_m_n(1, 1, |c: char| r#"0'\nrvtbf`""#.contains(c)),
    parse_basic_unicode_escapable,
    parse_entire_unicode_escapable,
  ))(input)
}

pub fn parse_elem_attr_value_content_within_quotation_mark(
  input: &str,
) -> ParseHTMLRes<&str, &str> {
  context(
    "elem attr value content within quotation mark",
    escaped(
      take_till(|c: char| r#""\"#.contains(c)),
      '\\',
      parse_string_escapable,
    ),
  )(input)
}

pub fn parse_elem_attr_value_content_within_apostrophe(input: &str) -> ParseHTMLRes<&str, &str> {
  context(
    "elem attr value content within apostrophe",
    escaped(
      take_till(|c: char| r#"'\"#.contains(c)),
      '\\',
      parse_string_escapable,
    ),
  )(input)
}

pub fn parse_elem_attr_value(input: &str) -> ParseHTMLRes<&str, &str> {
  context(
    "elem attr value",
    alt((
      parse_elem_attr_value_content_without_quote,
      delimited(
        tag("\""),
        parse_elem_attr_value_content_within_quotation_mark,
        tag("\""),
      ),
      delimited(
        tag("'"),
        parse_elem_attr_value_content_within_apostrophe,
        tag("'"),
      ),
    )),
  )(input)
}

pub fn parse_elem_tag_name(input: &str) -> ParseHTMLRes<&str, &str> {
  context("elem tag name", alphanumeric1)(input)
}

pub fn parse_elem_normal_attr(input: &str) -> ParseHTMLRes<&str, (&str, &str)> {
  context(
    "elem normal attr",
    separated_pair(
      parse_elem_attr_key,
      tuple((multispace0, tag("="), multispace0)),
      parse_elem_attr_value,
    ),
  )(input)
}

pub fn parse_elem_empty_attr(input: &str) -> ParseHTMLRes<&str, (&str, &str)> {
  context("elem empty attr", parse_elem_attr_key)(input)
    .map(|(next_input, result)| (next_input, (result, EMPTY_STR)))
}

pub fn parse_elem_attr(input: &str) -> ParseHTMLRes<&str, (&str, &str)> {
  context(
    "elem attr",
    alt((parse_elem_normal_attr, parse_elem_empty_attr)),
  )(input)
}

pub fn parse_elem_attrs(input: &str) -> ParseHTMLRes<&str, HashMap<&str, &str>> {
  context(
    "elem attrs",
    map(separated_list0(multispace1, parse_elem_attr), |s| {
      s.into_iter().collect::<HashMap<&str, &str>>()
    }),
  )(input)
}

pub fn parse_elem_start_tag_opening(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "elem starts tag opening",
    delimited(
      tag("<"),
      delimited(
        multispace0,
        tuple((
          parse_elem_tag_name,
          opt(preceded(multispace1, parse_elem_attrs)),
        )),
        multispace0,
      ),
      tag(">"),
    ),
  )(input)
  .map(|(next_input, result)| {
    let attributes = if let Some(attrs) = result.1 {
      attrs
    } else {
      HashMap::new()
    };
    (
      next_input,
      ParseHTMLSnippet::OpeningStartTag {
        tag_name: result.0,
        attributes,
      },
    )
  })
}

pub fn parse_elem_start_tag_closing(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "elem starts tag closing",
    delimited(
      tag("<"),
      delimited(
        multispace0,
        tuple((
          parse_elem_tag_name,
          opt(preceded(multispace1, parse_elem_attrs)),
        )),
        multispace0,
      ),
      tag("/>"),
    ),
  )(input)
  .map(|(next_input, result)| {
    let attributes = if let Some(attrs) = result.1 {
      attrs
    } else {
      HashMap::new()
    };
    (
      next_input,
      ParseHTMLSnippet::SelfClosingStartTag {
        tag_name: result.0,
        attributes,
      },
    )
  })
}

pub fn parse_elem_end_tag(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "elem end tag",
    delimited(
      tag("</"),
      delimited(multispace0, parse_elem_tag_name, multispace0),
      tag(">"),
    ),
  )(input)
  .map(|(next_input, result)| (next_input, ParseHTMLSnippet::EndTag { tag_name: result }))
}

pub fn parse_text(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context("text line ending", take_till1(|c: char| "<>".contains(c)))(input)
    .map(|(next_input, result)| (next_input, ParseHTMLSnippet::Text { text: result }))
}

pub fn parse_doctype_tag(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "doctype",
    delimited(
      tag("<!DOCTYPE"),
      tuple((
        multispace1,
        tag_no_case("html"),
        opt(tuple((
          multispace1,
          tag("SYSTEM"),
          multispace1,
          alt((
            tag("'about:legacy-compat'"),
            tag(r#""about:legacy-compat""#),
          )),
        ))),
        multispace0,
      )),
      tag(">"),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      ParseHTMLSnippet::DocTypeTag {
        legacy_compact: result.2.is_some(),
      },
    )
  })
}

pub fn parse_cdata_tag(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "cdata",
    delimited(tag("<![CDATA["), take_until("]]>"), tag("]]>")),
  )(input)
  .map(|(next_input, result)| (next_input, ParseHTMLSnippet::CDataTag { text: result }))
}

pub fn parse_comment_tag(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "comment",
    delimited(tag("<!--"), take_until("-->"), tag("-->")),
  )(input)
  .map(|(next_input, result)| (next_input, ParseHTMLSnippet::CommentTag { text: result }))
}

pub fn parse_snippet(input: &str) -> ParseHTMLRes<&str, ParseHTMLSnippet> {
  context(
    "snippet",
    alt((
      parse_elem_start_tag_opening,
      parse_elem_start_tag_closing,
      parse_elem_end_tag,
      parse_text,
      parse_comment_tag,
      parse_doctype_tag,
      parse_cdata_tag,
      fail,
    )),
  )(input)
}

#[tracing::instrument]
pub fn parse_html<'a>(input: &'a str) -> ParseHTMLRes<&'a str, Vec<DomNode>> {
  context("html", |i: &'a str| {
    let mut nodes = VecDeque::<ParseHTMLNode>::new();
    let mut prev_input = i;
    loop {
      if prev_input.is_empty() {
        break;
      }
      let (next_input, snippet) = parse_snippet(prev_input)?;
      {
        use ParseHTMLSnippet::*;
        match snippet {
          SelfClosingStartTag {
            tag_name,
            attributes,
          } => {
            let next_dom_node = DomNode::elem(
              tag_name,
              attributes
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
              vec![],
            );
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_dom_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_dom_node))
            }
          }
          OpeningStartTag {
            tag_name,
            attributes,
          } => {
            let next_dom_node = DomNode::elem(
              tag_name.to_string(),
              attributes
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
              vec![],
            );
            nodes.push_back(ParseHTMLNode::Opening(next_dom_node))
          }
          EndTag {
            tag_name: end_tag_name,
          } => {
            let next_dom_node = if let Some(ParseHTMLNode::Opening(start_node)) = nodes.pop_back() {
              if let DomNode {
                node_type:
                  DomNodeType::Element(ElementData {
                    tag_name: start_tag_name,
                    ..
                  }),
                ..
              } = &start_node
              {
                if start_tag_name == end_tag_name {
                  start_node
                } else {
                  return Err(NomErr::Error(VerboseError {
                    errors: vec![(prev_input, VerboseErrorKind::Nom(ErrorKind::Tag))],
                  }));
                }
              } else {
                return Err(NomErr::Error(VerboseError {
                  errors: vec![(prev_input, VerboseErrorKind::Nom(ErrorKind::Tag))],
                }));
              }
            } else {
              return Err(NomErr::Error(VerboseError {
                errors: vec![(prev_input, VerboseErrorKind::Nom(ErrorKind::Tag))],
              }));
            };
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_dom_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_dom_node))
            }
          }
          Text { text } => {
            let next_node = DomNode::text(text);
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_node))
            }
          }
          CommentTag { text } => {
            let next_node = DomNode::comment(text);
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_node))
            }
          }
          CDataTag { text } => {
            let next_node = DomNode::cdata(text);
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_node))
            }
          }
          DocTypeTag { legacy_compact } => {
            let next_node = DomNode::doctype(legacy_compact);
            if let Some(ParseHTMLNode::Opening(prev_node)) = nodes.back_mut() {
              prev_node.children.push(next_node)
            } else {
              nodes.push_back(ParseHTMLNode::Closing(next_node))
            }
          }
        };
      }
      if next_input == prev_input {
        break;
      }
      prev_input = next_input
    }
    let mut res: Vec<DomNode> = vec![];
    for node in nodes {
      if let ParseHTMLNode::Closing(dom_node) = node {
        res.push(dom_node)
      } else {
        tracing::error!("there is opening tag after processed end: {:#?}", node);
        return Err(NomErr::Error(VerboseError {
          errors: vec![(prev_input, VerboseErrorKind::Nom(ErrorKind::Fail))],
        }));
      }
    }
    Ok((prev_input, res))
  })(input)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::html::dom::Node;
  use maplit::hashmap;

  #[test]
  pub fn test_parse_html() {
    // install global collector configured based on RUST_LOG env var.
    tracing_subscriber::fmt::init();
    {
      let expect = Ok((
        "",
        vec![Node::elem(
          "html",
          hashmap! {},
          vec![
            Node::text("\n"),
            Node::elem(
              "head",
              hashmap! {},
              vec![
                Node::text("\n  "),
                Node::elem(
                  "meta",
                  hashmap! {"charset".into() => "utf-8".into()},
                  vec![],
                ),
                Node::text("\n  "),
                Node::elem(
                  "title",
                  hashmap! {},
                  vec![Node::text("\n    github.com\n  ")],
                ),
                Node::text("\n"),
              ],
            ),
            Node::text("\n"),
            Node::elem(
              "body",
              hashmap! {},
              vec![
                Node::text("\n  "),
                Node::elem("h1", hashmap! {}, vec![Node::text("我的第一个标题")]),
                Node::text("\n  "),
                Node::elem("p", hashmap! {}, vec![Node::text("我的第一个段落。")]),
                Node::text("\n"),
              ],
            ),
            Node::text("\n"),
          ],
        )],
      ));
      let found = parse_html(
        r#"<html>
<head>
  <meta charset="utf-8" />
  <title>
    github.com
  </title>
</head>
<body>
  <h1>我的第一个标题</h1>
  <p>我的第一个段落。</p>
</body>
</html>"#,
      );
      assert_eq!(found, expect);
    }

    {
      let expect = Ok((
        "",
        vec![
          Node::text("\n"),
          Node::doctype(true),
          Node::text("\n"),
          Node::elem(
            "html",
            hashmap! {},
            vec![
              Node::text("\n"),
              Node::elem(
                "head",
                hashmap! {},
                vec![
                  Node::text("\n  "),
                  Node::elem(
                    "meta",
                    hashmap! {"charset".into() => "utf-8".into()},
                    vec![],
                  ),
                  Node::text("\n  "),
                  Node::elem(
                    "title",
                    hashmap! {},
                    vec![Node::text("\n    github.com\n  ")],
                  ),
                  Node::text("\n  "),
                  Node::cdata("x<y3"),
                  Node::text("\n"),
                ],
              ),
              Node::text("\n"),
              Node::elem(
                "body",
                hashmap! {},
                vec![
                  Node::text("\n  "),
                  Node::comment("My favorite operators are!"),
                  Node::text("\n  "),
                  Node::elem("h1", hashmap! {}, vec![Node::text("我的第一个标题")]),
                  Node::text("\n  "),
                  Node::elem("p", hashmap! {}, vec![Node::text("我的第一个段落。")]),
                  Node::text("\n"),
                ],
              ),
              Node::text("\n"),
            ],
          ),
        ],
      ));
      let found = parse_html(
        r#"
<!DOCTYPE html SYSTEM "about:legacy-compat">
<html>
<head>
  <meta charset="utf-8" />
  <title>
    github.com
  </title>
  <![CDATA[x<y3]]>
</head>
<body>
  <!--My favorite operators are!-->
  <h1>我的第一个标题</h1>
  <p>我的第一个段落。</p>
</body>
</html>"#,
      );
      assert_eq!(found, expect);
    }
  }
}
