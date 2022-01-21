/**
 * [style specs](https://www.w3.org/TR/css-syntax-3/#typedef-delim-token)
 */
use nom::branch::alt;
use nom::bytes::complete::{
  escaped, tag, tag_no_case, take, take_till, take_till1, take_until, take_while_m_n,
};
use nom::character::complete::{multispace0, multispace1, none_of, one_of};
use nom::combinator::{fail, opt, recognize};
use nom::error::{context, VerboseError};
use nom::multi::{many0, separated_list0, separated_list1, many1};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{AsChar, IResult};

pub type ParseCSSRes<T, U> = IResult<T, U, VerboseError<T>>;

#[derive(Debug, PartialEq, Eq)]
pub enum CSSToken {
  Comment { text: String },
  Ident { text: String },
  Function { text: String },
  AtKeyword { text: String },
  Hash { text: String },
  String { text: String },
  Url { text: String },
  Number { number: String },
  Dimension { number: String, text: String },
  Percentage { number: String },
  Delim { text: String },
  Semicolon,
  Comma,
  Cdo,
  Cdc,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSComponentValue {
  FunctionBlock(CSSFunctionBlock),
  CurlyBracesBlock(CSSCurlyBracesBlock),
  RoundBracesBlock(CSSRoundBracesBlock),
  SquareBracesBlock(CSSSquareBracesBlock),
  PreservedToken(CSSToken),
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSFunctionBlock {
  pub function_name: String,
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSCurlyBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSRoundBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSSquareBracesBlock {
  pub components: Vec<CSSComponentValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSImportant {
  pub is_important: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSDeclaration {
  pub ident: String,
  pub components: Vec<CSSComponentValue>,
  pub important: CSSImportant,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSSelectorItem {
  Class { text: String },
  Id { text: String },
  Tag { text: String },
  All,
  Whitespaces,
  Unknown { text: String },
}

impl CSSSelectorItem {
  pub fn is_simple (&self) -> bool {
    match self {
      Self::Class { .. } | Self::Id { .. } | Self::Tag { .. } | Self::All => true,
      _ => false
    }
  }
}

pub type Specificity = (u32, u32, u32, u32);

#[derive(Debug, PartialEq, Eq)]
pub struct CSSSelector {
  items: Vec<CSSSelectorItem>
}

impl CSSSelector {
  pub fn specificity (&self) -> Option<Specificity> {
    if self.can_specificity() {
      let inline = 0u32;
      let mut id = 0u32;
      let mut class = 0u32;
      let mut tag = 0u32;
      for i in &self.items {
        match i {
          &CSSSelectorItem::Id { .. } => id += 1,
          &CSSSelectorItem::Tag { .. } => tag += 1,
          &CSSSelectorItem::Class { .. } => class += 1,
          _ => {}
        };
      }
      Some((inline, id, class, tag))
    } else {
      None
    }
  }

  pub fn is_simple (&self) -> bool {
    self.items.iter().all(|f| f.is_simple())
  }

  pub fn can_specificity (&self) -> bool {
    self.is_simple()
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSSelectorList {
  items: Vec<CSSSelector>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSQualifiedRuleSelectors {
  pub selectors: CSSSelectorList,
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSQualifiedRuleUnknown {
  pub components: Vec<CSSComponentValue>,
  pub block: CSSCurlyBracesBlock,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSQualifiedRule {
  Selectors(CSSQualifiedRuleSelectors),
  Unknown(CSSQualifiedRuleUnknown),
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSAtRuleFontFace {
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSAtRuleKeyframes {
  pub keyframes_name: String,
  pub keyframes: CSSKeyframeRuleList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSAtRuleUnknown {
  pub at_keyword: String,
  pub components: Vec<CSSComponentValue>,
  pub block: Option<CSSCurlyBracesBlock>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSAtRule {
  FontFace(CSSAtRuleFontFace),
  Keyframes(CSSAtRuleKeyframes),
  Unknown(CSSAtRuleUnknown),
  /* not implement
  // Import
  // Media,
  // FontFeatureValues,
  // CountStyle
  // Document
  // Page
  // Namespace
  // Viewport
   **/
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSRuleList {
  pub items: Vec<CSSRuleListItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSRuleListItem {
  AtRule(CSSAtRule),
  QualifiedRule(CSSQualifiedRule),
  Whitespaces,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSKeyframeRule {
  pub selectors: Vec<CSSToken>,
  pub declaration_list: CSSDeclarationList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSKeyframeRuleList {
  pub items: Vec<CSSKeyframeRule>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSDeclarationList {
  pub items: Vec<CSSDeclarationListItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSDeclarationListItem {
  AtRule(CSSAtRule),
  Declaration(CSSDeclaration),
}

#[derive(Debug, PartialEq, Eq)]
pub struct CSSStylesheet {
  pub items: Vec<CSSStylesheetItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CSSStylesheetItem {
  Whitespaces,
  QualifiedRule(CSSQualifiedRule),
  AtRule(CSSAtRule),
  Cdo,
  Cdc,
}

pub fn parse_normal_escapable(input: &str) -> ParseCSSRes<&str, &str> {
  alt((
    recognize(one_of("rnft")),
    terminated(
      take_while_m_n(1, 6, |c: char| c.is_hex_digit()),
      multispace0,
    ),
  ))(input)
}

pub fn parse_escape(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "escape",
    recognize(preceded(tag("\\"), parse_normal_escapable)),
  )(input)
}

pub fn parse_number(input: &str) -> ParseCSSRes<&str, &str> {
  context("number", recognize_float)(input)
}

pub fn parse_string_escapable_within_quotation_mark(input: &str) -> ParseCSSRes<&str, &str> {
  alt((parse_normal_escapable, tag("\"")))(input)
}

pub fn parse_string_within_quotation_mark(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "string within quotation mark",
    delimited(
      tag("\""),
      alt((
        escaped(
          none_of("\\\"\r\n\x0C"),
          '\\',
          parse_string_escapable_within_quotation_mark,
        ),
        tag(""),
      )),
      tag("\""),
    ),
  )(input)
}

pub fn parse_string_escapable_within_apostrophe(input: &str) -> ParseCSSRes<&str, &str> {
  alt((parse_normal_escapable, tag("'")))(input)
}

pub fn parse_string_within_apostrophe(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "string within apostrophe",
    delimited(
      tag("'"),
      alt((
        escaped(
          none_of("\\'\r\n\x0C"),
          '\\',
          parse_string_escapable_within_apostrophe,
        ),
        tag(""),
      )),
      tag("'"),
    ),
  )(input)
}

pub fn parse_string(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "string",
    alt((
      parse_string_within_quotation_mark,
      parse_string_within_apostrophe,
      fail,
    )),
  )(input)
}

pub fn parse_normal_code_points(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "normal code points",
    take_till(|c: char| !c.is_alphanumeric() && !"-_".contains(c) && c.is_ascii()),
  )(input)
}

pub fn parse_ident(input: &str) -> ParseCSSRes<&str, &str> {
  context(
    "ident",
    recognize(pair(
      alt((
        tag("--"),
        recognize(pair(
          opt(tag("-")),
          escaped(
            take_till1(|c: char| !c.is_alphanumeric() && c != '_' && c.is_ascii()),
            '\\',
            parse_normal_escapable,
          ),
        )),
      )),
      opt(recognize(escaped(
        parse_normal_code_points,
        '\\',
        parse_normal_escapable,
      ))),
    )),
  )(input)
}

pub fn parse_comment_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("comment", delimited(tag("/*"), take_until("*/"), tag("*/")))(input).map(
    |(next_input, result)| {
      (
        next_input,
        CSSToken::Comment {
          text: result.into(),
        },
      )
    },
  )
}

pub fn parse_number_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("number token", parse_number)(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Number {
        number: result.into(),
      },
    )
  })
}

pub fn parse_percentage_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("percentag token", terminated(parse_number, tag("%")))(input).map(
    |(next_input, result)| {
      (
        next_input,
        CSSToken::Percentage {
          number: result.into(),
        },
      )
    },
  )
}

pub fn parse_string_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("string token", parse_string)(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::String {
        text: result.into(),
      },
    )
  })
}

pub fn parse_hash_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context(
    "hash token",
    preceded(
      tag("#"),
      escaped(parse_normal_code_points, '\\', parse_normal_escapable),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Hash {
        text: result.into(),
      },
    )
  })
}

pub fn parse_ident_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("ident token", parse_ident)(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Ident {
        text: result.into(),
      },
    )
  })
}

pub fn parse_function_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("function token", terminated(parse_ident, tag("(")))(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Function {
        text: result.into(),
      },
    )
  })
}

pub fn parse_at_keyword_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("at keyword token", preceded(tag("@"), parse_ident))(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::AtKeyword {
        text: result.into(),
      },
    )
  })
}

pub fn parse_url_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context(
    "url token",
    delimited(
      tag("url("),
      delimited(
        multispace0,
        alt((
          parse_string,
          escaped(
            take_till(|c: char| "\"'()\\".contains(c) || c.is_whitespace() || c.is_control()),
            '\\',
            parse_normal_escapable,
          ),
          tag(""),
        )),
        multispace0,
      ),
      tag(")"),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Url {
        text: result.into(),
      },
    )
  })
}

pub fn parse_dimension_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("dimension token", pair(parse_number, parse_ident))(input).map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Dimension {
        number: result.0.into(),
        text: result.1.into(),
      },
    )
  })
}

pub fn parse_cdo_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("CDO token", tag("<!--"))(input).map(|(next_input, _)| (next_input, CSSToken::Cdo))
}

pub fn parse_cdc_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("CDC token", tag("-->"))(input).map(|(next_input, _)| (next_input, CSSToken::Cdc))
}

pub fn parse_delim_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context(
    "delim",
    alt((
      tag("#"),
      tag("+"),
      tag("-"),
      tag("."),
      tag("<"),
      tag("@"),
      tag("\\"),
      tag(":"),
    )),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSToken::Delim {
        text: result.into(),
      },
    )
  })
}

pub fn parse_comma_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("comma", tag(","))(input).map(|(next_input, _)| (next_input, CSSToken::Comma))
}

pub fn parse_semicolon_token(input: &str) -> ParseCSSRes<&str, CSSToken> {
  context("semicolon", tag(";"))(input).map(|(next_input, _)| (next_input, CSSToken::Semicolon))
}

pub fn parse_preserved_token<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSToken> {
  context(
    "preserved token",
    alt((
      parse_hash_token,
      parse_string_token,
      parse_url_token,
      parse_at_keyword_token,
      parse_dimension_token,
      parse_percentage_token,
      parse_number_token,
      parse_ident_token,
      parse_comment_token,
      parse_semicolon_token,
      parse_comma_token,
      parse_delim_token,
    )),
  )(input)
}

pub fn parse_component_value<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSComponentValue> {
  context(
    "component value",
    alt((
      |i: &'a str| {
        parse_round_braces_block(i)
          .map(|(next_i, r)| (next_i, CSSComponentValue::RoundBracesBlock(r)))
      },
      |i: &'a str| {
        parse_square_braces_block(i)
          .map(|(next_i, r)| (next_i, CSSComponentValue::SquareBracesBlock(r)))
      },
      |i: &'a str| {
        parse_curly_braces_block(i)
          .map(|(next_i, r)| (next_i, CSSComponentValue::CurlyBracesBlock(r)))
      },
      |i: &'a str| {
        parse_function_block(i).map(|(next_i, r)| (next_i, CSSComponentValue::FunctionBlock(r)))
      },
      |i: &'a str| {
        parse_preserved_token(i).map(|(next_i, r)| (next_i, CSSComponentValue::PreservedToken(r)))
      },
      fail,
    )),
  )(input)
}

pub fn parse_component_values(input: &str) -> ParseCSSRes<&str, Vec<CSSComponentValue>> {
  context(
    "component values",
    delimited(
      multispace0,
      separated_list0(multispace1, parse_component_value),
      multispace0,
    ),
  )(input)
}

pub fn parse_curly_braces_block(input: &str) -> ParseCSSRes<&str, CSSCurlyBracesBlock> {
  context(
    "curly braces block",
    delimited(tag("{"), parse_component_values, tag("}")),
  )(input)
  .map(|(next_input, result)| (next_input, CSSCurlyBracesBlock { components: result }))
}

pub fn parse_round_braces_block(input: &str) -> ParseCSSRes<&str, CSSRoundBracesBlock> {
  context(
    "round braces block",
    delimited(tag("("), parse_component_values, tag("r")),
  )(input)
  .map(|(next_input, result)| (next_input, CSSRoundBracesBlock { components: result }))
}

pub fn parse_square_braces_block(input: &str) -> ParseCSSRes<&str, CSSSquareBracesBlock> {
  context(
    "square braces block",
    delimited(tag("["), parse_component_values, tag("]")),
  )(input)
  .map(|(next_input, result)| (next_input, CSSSquareBracesBlock { components: result }))
}

pub fn parse_function_block(input: &str) -> ParseCSSRes<&str, CSSFunctionBlock> {
  context(
    "function block",
    terminated(pair(parse_function_token, parse_component_values), tag(")")),
  )(input)
  .map(|(next_input, result)| {
    if let CSSToken::Function {
      text: function_name,
    } = result.0
    {
      let block = CSSFunctionBlock {
        function_name,
        components: result.1,
      };
      (next_input, block)
    } else {
      unreachable!()
    }
  })
}

pub fn parse_important<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSImportant> {
  context(
    "important",
    alt((
      |i: &'a str| {
        preceded(
          tag("!"),
          delimited(multispace0, tag_no_case("important"), multispace0),
        )(i)
        .map(|(next_i, _)| (next_i, CSSImportant { is_important: true }))
      },
      |i: &'a str| {
        take(0usize)(i).map(|(next_i, _)| {
          (
            next_i,
            CSSImportant {
              is_important: false,
            },
          )
        })
      },
    )),
  )(input)
}

pub fn parse_declaration<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSDeclaration> {
  context(
    "declaration",
    tuple((
      parse_ident_token,
      multispace0,
      tag(":"),
      parse_component_values,
      parse_important,
    )),
  )(input)
  .map(|(next_input, result)| {
    if let CSSToken::Ident { text: ident, .. } = result.0 {
      let declaration = CSSDeclaration {
        ident,
        components: result.3,
        important: result.4,
      };
      (next_input, declaration)
    } else {
      unreachable!()
    }
  })
}

pub fn parse_selector<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSSelector> {
  context(
    "select",
    many1(
      |seg: &'a str| {
        alt((
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, r1) = parse_hash_token(i)?;
            if let CSSToken::Hash { text: ident } = r1 {
              Ok((next_i1, CSSSelectorItem::Id { text: ident }))
            } else {
              unreachable!()
            }
          },
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, r1) = preceded(tag("."), parse_ident_token)(i)?;
            if let CSSToken::Ident { text: ident } = r1 {
              Ok((next_i1, CSSSelectorItem::Class { text: ident }))
            } else {
              unreachable!()
            }
          },
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, r1) = parse_ident_token(i)?;
            if let CSSToken::Ident { text: ident } = r1 {
              Ok((next_i1, CSSSelectorItem::Tag { text: ident }))
            } else {
              unreachable!()
            }
          },
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, _) = tag("*")(i)?;
            Ok((next_i1, CSSSelectorItem::All))
          },
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, _) = multispace1(i)?;
            Ok((next_i1, CSSSelectorItem::Whitespaces))
          },
          |i: &'a str| -> ParseCSSRes<&'a str, CSSSelectorItem> {
            let (next_i1, r1) = take_till1(|c: char| ",{".contains(c) || c.is_whitespace())(i)?;
            Ok((next_i1, CSSSelectorItem::Unknown { text: r1.into() }))
          },
        ))(seg)
      }
    ),
  )(input)
    .map(|(next_input, mut result)| {
      while let Some(CSSSelectorItem::Whitespaces) = result.last() {
        result.pop();
      };
      (next_input, CSSSelector { items: result })
    })
}

pub fn parse_selectors(input: &str) -> ParseCSSRes<&str, CSSSelectorList> {
  context(
    "selectors",
    separated_list0(
      tag(","),
      delimited(multispace0, parse_selector, multispace0),
    ),
  )(input)
  .map(|(next_input, result)| (next_input, CSSSelectorList { items: result }))
}

pub fn parse_qualified_rule_selectors(input: &str) -> ParseCSSRes<&str, CSSQualifiedRule> {
  context(
    "qualified rule selectors",
    pair(
      terminated(parse_selectors, multispace0),
      delimited(
        tag("{"),
        delimited(multispace0, parse_declaration_list, multispace0),
        tag("}"),
      ),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSQualifiedRule::Selectors(CSSQualifiedRuleSelectors {
        selectors: result.0,
        declaration_list: result.1,
      }),
    )
  })
}

pub fn parse_qualified_rule_unknown(input: &str) -> ParseCSSRes<&str, CSSQualifiedRule> {
  context(
    "qualified rule unknown",
    pair(parse_component_values, parse_curly_braces_block),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSQualifiedRule::Unknown(CSSQualifiedRuleUnknown {
        components: result.0,
        block: result.1,
      }),
    )
  })
}

pub fn parse_qualified_rule(input: &str) -> ParseCSSRes<&str, CSSQualifiedRule> {
  context(
    "qualified rule",
    alt((parse_qualified_rule_selectors, parse_qualified_rule_unknown)),
  )(input)
}

pub fn parse_keyframe_rule(input: &str) -> ParseCSSRes<&str, CSSKeyframeRule> {
  context(
    "keyframe rule",
    pair(
      separated_list1(
        tag(","),
        delimited(multispace0, parse_preserved_token, multispace0),
      ),
      delimited(
        tag("{"),
        delimited(multispace0, parse_declaration_list, multispace0),
        tag("}"),
      ),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSKeyframeRule {
        selectors: result.0,
        declaration_list: result.1,
      },
    )
  })
}

pub fn parse_keyframe_rules(input: &str) -> ParseCSSRes<&str, CSSKeyframeRuleList> {
  context(
    "keyframes rules",
    many0(preceded(multispace0, parse_keyframe_rule)),
  )(input)
  .map(|(next_input, result)| (next_input, CSSKeyframeRuleList { items: result }))
}

pub fn parse_at_rule_keyframes<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSAtRule> {
  context(
    "keyframes",
    pair(
      preceded(multispace1, parse_ident_token),
      preceded(
        multispace0,
        delimited(
          tag("{"),
          delimited(multispace0, parse_keyframe_rules, multispace0),
          tag("}"),
        ),
      ),
    ),
  )(input)
  .map(|(next_input, result)| {
    if let CSSToken::Ident {
      text: keyframes_name,
    } = result.0
    {
      let rule = CSSAtRule::Keyframes(CSSAtRuleKeyframes {
        keyframes_name,
        keyframes: result.1,
      });
      (next_input, rule)
    } else {
      unreachable!()
    }
  })
}

pub fn parse_at_rule_font_face<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSAtRule> {
  context(
    "at rule font-face",
    preceded(
      multispace0,
      delimited(
        tag("{"),
        delimited(multispace0, parse_declaration_list, multispace0),
        tag("}"),
      ),
    ),
  )(input)
  .map(|(next_input, result)| {
    (
      next_input,
      CSSAtRule::FontFace(CSSAtRuleFontFace {
        declaration_list: result,
      }),
    )
  })
}

pub fn parse_at_rule_unknown<'a>(
  input: &'a str,
  at_keyword: String,
) -> ParseCSSRes<&'a str, CSSAtRule> {
  context(
    "at rule unknown",
    tuple((
      parse_component_values,
      alt((
        |i: &'a str| parse_curly_braces_block(i).map(|(next_i, r)| (next_i, Some(r))),
        |i: &'a str| tag(";")(i).map(|(next_i, _)| (next_i, None)),
        fail,
      )),
    )),
  )(input)
  .map(|(next_input, result)| {
    let at_rule = CSSAtRule::Unknown(CSSAtRuleUnknown {
      at_keyword,
      components: result.0,
      block: result.1,
    });
    (next_input, at_rule)
  })
}

pub fn parse_at_rule<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSAtRule> {
  context("at rule", |i: &'a str| -> ParseCSSRes<&'a str, CSSAtRule> {
    let (next_i1, r1) = parse_at_keyword_token(i)?;
    if let CSSToken::AtKeyword { text: at_keyword } = r1 {
      match &at_keyword as &str {
        "keyframes" => parse_at_rule_keyframes(next_i1),
        "font-face" => parse_at_rule_font_face(next_i1),
        at_keyword => parse_at_rule_unknown(next_i1, at_keyword.into()),
      }
    } else {
      unreachable!()
    }
  })(input)
}

pub fn parse_rule_list<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSRuleList> {
  context(
    "rule list",
    many0(alt((
      |i: &'a str| {
        parse_qualified_rule(i).map(|(next_i, r)| (next_i, CSSRuleListItem::QualifiedRule(r)))
      },
      |i: &'a str| parse_at_rule(i).map(|(next_i, r)| (next_i, CSSRuleListItem::AtRule(r))),
      |i: &'a str| multispace1(i).map(|(next_i, _)| (next_i, CSSRuleListItem::Whitespaces)),
      fail,
    ))),
  )(input)
  .map(|(next_input, result)| (next_input, CSSRuleList { items: result }))
}

pub fn parse_declaration_list<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSDeclarationList> {
  context(
    "declaration",
    preceded(
      multispace0,
      alt((
        |i: &'a str| -> ParseCSSRes<&'a str, CSSDeclarationList> {
          let (next_i1, r1) = parse_at_rule(i)?;
          let (next_i2, r2) = parse_declaration_list(next_i1)?;
          let mut declaration_list = vec![];
          declaration_list.push(CSSDeclarationListItem::AtRule(r1));
          declaration_list.extend(r2.items.into_iter());
          Ok((
            next_i2,
            CSSDeclarationList {
              items: declaration_list,
            },
          ))
        },
        |i: &'a str| -> ParseCSSRes<&'a str, CSSDeclarationList> {
          let (next_i1, r1) = opt(parse_declaration)(i)?;
          let (next_i2, r2) = opt(preceded(tag(";"), parse_declaration_list))(next_i1)?;
          let mut declaration_list = vec![];
          if let Some(r) = r1 {
            declaration_list.push(CSSDeclarationListItem::Declaration(r));
          };
          if let Some(CSSDeclarationList { items }) = r2 {
            declaration_list.extend(items.into_iter())
          };
          Ok((
            next_i2,
            CSSDeclarationList {
              items: declaration_list,
            },
          ))
        },
        |i: &'a str| -> ParseCSSRes<&'a str, CSSDeclarationList> {
          tag("")(i).map(|(next_i, _)| (next_i, CSSDeclarationList { items: vec![] }))
        },
      )),
    ),
  )(input)
}

pub fn parse_stylesheet_item<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSStylesheetItem> {
  context(
    "stylesheet item",
    alt((
      |i: &'a str| parse_cdo_token(i).map(|(next_i, _)| (next_i, CSSStylesheetItem::Cdo)),
      |i: &'a str| parse_cdc_token(i).map(|(next_i, _)| (next_i, CSSStylesheetItem::Cdc)),
      |i: &'a str| multispace1(i).map(|(next_i, _)| (next_i, CSSStylesheetItem::Whitespaces)),
      |i: &'a str| parse_at_rule(i).map(|(next_i, r)| (next_i, CSSStylesheetItem::AtRule(r))),
      |i: &'a str| {
        parse_qualified_rule(i).map(|(next_i, r)| (next_i, CSSStylesheetItem::QualifiedRule(r)))
      },
    )),
  )(input)
}

pub fn parse_stylesheet<'a>(input: &'a str) -> ParseCSSRes<&'a str, CSSStylesheet> {
  context(
    "stylesheet",
    |i: &'a str| -> ParseCSSRes<&'a str, CSSStylesheet> {
      let mut items = vec![];
      let mut prev_i = i;
      loop {
        if prev_i.len() == 0 {
          break;
        }
        let (next_i, r) = parse_stylesheet_item(prev_i)?;
        if prev_i == next_i {
          break;
        }
        items.push(r);
        prev_i = next_i;
      }
      Ok((prev_i, CSSStylesheet { items }))
    },
  )(input)
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_parse_tokens() {
    /* comment */
    assert_eq!(
      parse_comment_token(r"/* aaaaaa */"),
      Ok((
        "",
        CSSToken::Comment {
          text: " aaaaaa ".into()
        }
      ))
    );
    /* ident */
    assert_eq!(
      parse_ident_token(r#"width"#),
      Ok((
        "",
        CSSToken::Ident {
          text: "width".into()
        }
      ))
    );
    assert_eq!(
      parse_ident_token(r#"--header"#),
      Ok((
        "",
        CSSToken::Ident {
          text: "--header".into()
        }
      ))
    );
    /* function */
    assert_eq!(
      parse_function_token("calc("),
      Ok((
        "",
        CSSToken::Function {
          text: "calc".into()
        }
      ))
    );
    /* at keyword */
    assert_eq!(
      parse_at_keyword_token(r#"@media"#),
      Ok((
        "",
        CSSToken::AtKeyword {
          text: "media".into()
        }
      ))
    );
    /* hash */
    assert_eq!(
      parse_hash_token("#id"),
      Ok(("", CSSToken::Hash { text: "id".into() }))
    );
    /* string */
    assert_eq!(
      parse_string_token(r#""""#),
      Ok(("", CSSToken::String { text: "".into() }))
    );
    assert_eq!(
      parse_string_token(r#""aa\"""#),
      Ok((
        "",
        CSSToken::String {
          text: "aa\\\"".into()
        }
      ))
    );
    assert_eq!(
      parse_string_token(r#"''"#),
      Ok(("", CSSToken::String { text: "".into() }))
    );
    assert_eq!(
      parse_string_token(r#"'\t'"#),
      Ok(("", CSSToken::String { text: "\\t".into() }))
    );
    assert_eq!(
      parse_string_token(r#"'\''"#),
      Ok(("", CSSToken::String { text: "\\'".into() }))
    );
    /* url */
    assert_eq!(
      parse_url_token("url( http://baidu.com )"),
      Ok((
        "",
        CSSToken::Url {
          text: "http://baidu.com".into()
        }
      ))
    );
    assert_eq!(
      parse_url_token("url()"),
      Ok(("", CSSToken::Url { text: "".into() }))
    );
    assert_eq!(
      parse_url_token(r#"url( "http://baidu.com" )"#),
      Ok((
        "",
        CSSToken::Url {
          text: "http://baidu.com".into()
        }
      ))
    );
    assert_eq!(
      parse_url_token(r#"url( '//baidu.com' )"#),
      Ok((
        "",
        CSSToken::Url {
          text: "//baidu.com".into()
        }
      ))
    );
    /* number */
    assert_eq!(
      parse_number_token("+3.0e-6"),
      Ok((
        "",
        CSSToken::Number {
          number: "+3.0e-6".into()
        }
      ))
    );
    assert!(parse_number_token("aaa+3.0e-6").is_err());
    /* dimension */
    assert_eq!(
      parse_dimension_token("3n"),
      Ok((
        "",
        CSSToken::Dimension {
          number: "3".into(),
          text: "n".into()
        }
      ))
    );
    /* percentage */
    assert_eq!(
      parse_percentage_token("100%"),
      Ok((
        "",
        CSSToken::Percentage {
          number: "100".into()
        }
      ))
    );
    /* msic */
    assert_eq!(
      parse_delim_token("+"),
      Ok(("", CSSToken::Delim { text: "+".into() }))
    );
    assert_eq!(parse_semicolon_token(";"), Ok(("", CSSToken::Semicolon)));
    assert_eq!(parse_comma_token(","), Ok(("", CSSToken::Comma)));
    assert_eq!(parse_cdo_token("<!--"), Ok(("", CSSToken::Cdo)));
    assert_eq!(parse_cdc_token("-->"), Ok(("", CSSToken::Cdc)));
  }

  #[test]
  fn test_parse_declaration() {
    let found = parse_declaration(r#"width: 100% !important"#);
    let expected = Ok((
      "",
      CSSDeclaration {
        ident: "width".into(),
        components: vec![CSSComponentValue::PreservedToken(CSSToken::Percentage {
          number: "100".into(),
        })],
        important: CSSImportant { is_important: true },
      },
    ));
    assert_eq!(found, expected);
  }

  #[test]
  fn test_parse_selector_qualified_rule_selector() {
    let css_content = r#"tag, .my-class {
      width: 100%;
      height: 10px;
    }"#;
    let found = parse_qualified_rule_selectors(css_content);
    let expected = Ok((
      "",
      CSSQualifiedRule::Selectors(CSSQualifiedRuleSelectors {
        selectors: CSSSelectorList {
          items: vec![
            CSSSelector {
              items: vec![CSSSelectorItem::Tag { text: "tag".into() }]
            },
            CSSSelector {
              items: vec![CSSSelectorItem::Class {
                text: "my-class".into(),
              }]
            }
          ],
        },
        declaration_list: CSSDeclarationList {
          items: vec![
            CSSDeclarationListItem::Declaration(CSSDeclaration {
              ident: "width".into(),
              components: vec![CSSComponentValue::PreservedToken(CSSToken::Percentage {
                number: "100".into(),
              })],
              important: CSSImportant {
                is_important: false,
              },
            }),
            CSSDeclarationListItem::Declaration(CSSDeclaration {
              ident: "height".into(),
              components: vec![CSSComponentValue::PreservedToken(CSSToken::Dimension {
                number: "10".into(),
                text: "px".into(),
              })],
              important: CSSImportant {
                is_important: false,
              },
            }),
          ],
        },
      }),
    ));
    assert_eq!(found, expected);
  }

  #[test]
  fn test_parse_naive_css() {
    use super::CSSAtRule::*;
    use super::CSSComponentValue::*;
    use super::CSSDeclarationListItem::*;
    use super::CSSQualifiedRule::*;
    use super::CSSSelectorItem::*;
    use super::CSSStylesheetItem::*;
    use super::CSSToken::*;
    use super::CSSStylesheetItem::Whitespaces;
    let css_content = r#"
      tag.my-class1, .my-class {
        width: 100%;
        height: 10px;
      }

      #name {
        animation: my-ani 1s infinite;
        position: absolute;
      }

      @keyframes my-ani {
        from {
          transform: scale(0.1);
        }
        50% {
          transform: scale(1) translateX(100px);
        }
        to {
          transform: scale(1);
        }
      }
    "#;

    let found = parse_stylesheet(css_content);
    let expected = Ok((
      "",
      CSSStylesheet {
        items: vec![
          Whitespaces,
          QualifiedRule(Selectors(CSSQualifiedRuleSelectors {
            selectors: CSSSelectorList {
              items: vec![
                CSSSelector {
                  items: vec![
                    Tag { text: "tag".into() },
                    Class { text: "my-class1".into() }
                  ]
                },
                CSSSelector {
                  items: vec![
                    Class {
                      text: "my-class".into(),
                    }
                  ]
                }
              ],
            },
            declaration_list: CSSDeclarationList {
              items: vec![
                Declaration(CSSDeclaration {
                  ident: "width".into(),
                  components: vec![PreservedToken(Percentage {
                    number: "100".into(),
                  })],
                  important: CSSImportant {
                    is_important: false,
                  },
                }),
                Declaration(CSSDeclaration {
                  ident: "height".into(),
                  components: vec![PreservedToken(Dimension {
                    number: "10".into(),
                    text: "px".into(),
                  })],
                  important: CSSImportant {
                    is_important: false,
                  },
                }),
              ],
            },
          })),
          Whitespaces,
          QualifiedRule(Selectors(CSSQualifiedRuleSelectors {
            selectors: CSSSelectorList {
              items: vec![
                CSSSelector {
                  items: vec![
                    Id {
                      text: "name".into(),
                    }
                  ]
                }
              ],
            },
            declaration_list: CSSDeclarationList {
              items: vec![
                Declaration(CSSDeclaration {
                  ident: "animation".into(),
                  components: vec![
                    PreservedToken(Ident {
                      text: "my-ani".into(),
                    }),
                    PreservedToken(Dimension {
                      number: "1".into(),
                      text: "s".into(),
                    }),
                    PreservedToken(Ident {
                      text: "infinite".into(),
                    }),
                  ],
                  important: CSSImportant {
                    is_important: false,
                  },
                }),
                Declaration(CSSDeclaration {
                  ident: "position".into(),
                  components: vec![PreservedToken(Ident {
                    text: "absolute".into(),
                  })],
                  important: CSSImportant {
                    is_important: false,
                  },
                }),
              ],
            },
          })),
          Whitespaces,
          CSSStylesheetItem::AtRule(Keyframes(CSSAtRuleKeyframes {
            keyframes_name: "my-ani".into(),
            keyframes: CSSKeyframeRuleList {
              items: vec![
                CSSKeyframeRule {
                  selectors: vec![Ident {
                    text: "from".into(),
                  }],
                  declaration_list: CSSDeclarationList {
                    items: vec![Declaration(CSSDeclaration {
                      ident: "transform".into(),
                      components: vec![FunctionBlock(CSSFunctionBlock {
                        function_name: "scale".into(),
                        components: vec![PreservedToken(Number {
                          number: "0.1".into(),
                        })],
                      })],
                      important: CSSImportant {
                        is_important: false,
                      },
                    })],
                  },
                },
                CSSKeyframeRule {
                  selectors: vec![Percentage {
                    number: "50".into(),
                  }],
                  declaration_list: CSSDeclarationList {
                    items: vec![Declaration(CSSDeclaration {
                      ident: "transform".into(),
                      components: vec![
                        FunctionBlock(CSSFunctionBlock {
                          function_name: "scale".into(),
                          components: vec![PreservedToken(Number { number: "1".into() })],
                        }),
                        FunctionBlock(CSSFunctionBlock {
                          function_name: "translateX".into(),
                          components: vec![PreservedToken(Dimension {
                            number: "100".into(),
                            text: "px".into(),
                          })],
                        }),
                      ],
                      important: CSSImportant {
                        is_important: false,
                      },
                    })],
                  },
                },
                CSSKeyframeRule {
                  selectors: vec![Ident { text: "to".into() }],
                  declaration_list: CSSDeclarationList {
                    items: vec![Declaration(CSSDeclaration {
                      ident: "transform".into(),
                      components: vec![FunctionBlock(CSSFunctionBlock {
                        function_name: "scale".into(),
                        components: vec![PreservedToken(Number { number: "1".into() })],
                      })],
                      important: CSSImportant {
                        is_important: false,
                      },
                    })],
                  },
                },
              ],
            },
          })),
          Whitespaces,
        ],
      },
    ));
    assert_eq!(found, expected);
  }
}
