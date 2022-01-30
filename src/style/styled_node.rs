use crate::css::cssom::{
  CSSComponentValue, CSSDeclaration, CSSDeclarationListItem, CSSDisplay, CSSQualifiedRule,
  CSSQualifiedRuleSelectors, CSSSelector, CSSSelectorItem, CSSSpecificity, CSSStylesheet,
  CSSStylesheetItem,
};
use crate::html::dom::{ElementData, Node, NodeType};
use std::collections::HashMap;
use std::ops::FnOnce;
use std::cell::RefCell;
use std::sync::Arc;

lazy_static! {
  static ref TAG_NAME_DEFAULT_DISPLAY: HashMap<&'static str, CSSDisplay> = hashmap! {
    "span" => CSSDisplay::Inline,
    "img" => CSSDisplay::Replace,
    "video" => CSSDisplay::Replace,
    "audio" => CSSDisplay::Replace,
    "canvas" => CSSDisplay::Replace,
    "input" => CSSDisplay::Inline,
    "textarea" => CSSDisplay::Inline,
    "option" => CSSDisplay::Replace,
    "b" => CSSDisplay::Inline,
    "i" => CSSDisplay::Inline,
    "em" => CSSDisplay::Inline,
    "strong" => CSSDisplay::Inline,
    "a" => CSSDisplay::Inline,
    "label" => CSSDisplay::Inline,
    "button" => CSSDisplay::Inline,
    "select" => CSSDisplay::Inline,
    "script" => CSSDisplay::None,
    "style" => CSSDisplay::None,
    "head" => CSSDisplay::None
  };
}

pub type PropertyMap = HashMap<String, CSSDeclaration>;
pub type MatchedRule<'a> = (Option<CSSSpecificity>, &'a CSSQualifiedRuleSelectors);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StyledNode {
  pub node: Arc<RefCell<Node>>,
  pub specified_values: PropertyMap,
  pub children: Vec<Arc<RefCell<StyledNode>>>,
}

impl StyledNode {
  pub fn declaration(&self, name: &str) -> Option<&CSSDeclaration> {
    self.specified_values.get(name)
  }

  pub fn declaration_nth_component(&self, name: &str, idx: usize) -> Option<&CSSComponentValue> {
    self.declaration(name).and_then(|d| d.nth_component(idx))
  }

  pub fn default_display(&self) -> CSSDisplay {
    match self.node.borrow().node_type {
      NodeType::Element(ref data) => TAG_NAME_DEFAULT_DISPLAY
        .get(&data.tag_name as &str)
        .map_or(CSSDisplay::Block, |c| c.clone()),
      NodeType::Text { .. } => CSSDisplay::Inline,
      _ => CSSDisplay::None,
    }
  }

  pub fn display(&self) -> CSSDisplay {
    if let Some(value) = self
      .declaration_nth_component("display", 0)
      .and_then(CSSComponentValue::get_ident)
    {
      match value as &str {
        "inline" => CSSDisplay::Inline,
        "block" => CSSDisplay::Block,
        "none" => CSSDisplay::None,
        _ => self.default_display(),
      }
    } else {
      self.default_display()
    }
  }

  pub fn declaration_nth_components_map<R, FM: FnOnce(&CSSComponentValue) -> Option<R> + Copy>(
    &self,
    choices: &[(&str, usize)],
    fmap: FM,
  ) -> Option<R> {
    for &(name, nth) in choices.iter().rev() {
      let curr_value = self.declaration_nth_component(name, nth).and_then(fmap);
      if curr_value.is_some() {
        return curr_value;
      }
    }
    None
  }

  pub fn declaration_nth_component_map<R, FM: FnOnce(&CSSComponentValue) -> Option<R>>(
    &self,
    name: &str,
    nth: usize,
    fmap: FM,
  ) -> Option<R> {
    self.declaration_nth_component(name, nth).and_then(fmap)
  }

  pub fn declaration_nth_component_map_or_else<
    R,
    FM: FnOnce(&CSSComponentValue) -> Option<R>,
    FD: FnOnce() -> R,
  >(
    &self,
    name: &str,
    nth: usize,
    fmap: FM,
    default_fn: FD,
  ) -> R {
    self
      .declaration_nth_component_map(name, nth, fmap)
      .unwrap_or_else(default_fn)
  }

  pub fn declaration_nth_components_map_or_else<
    R,
    FM: Fn(&CSSComponentValue) -> Option<R> + Copy,
    FD: FnOnce() -> R,
  >(
    &self,
    choices: &[(&str, usize)],
    fmap: FM,
    default_fn: FD,
  ) -> R {
    for &(name, nth) in choices.iter().rev() {
      let curr_value = self.declaration_nth_component(name, nth).and_then(fmap);
      if let Some(v) = curr_value {
        return v;
      }
    }
    default_fn()
  }
}

pub fn style_tree(root: Arc<RefCell<Node>>, stylesheet: &CSSStylesheet) -> Arc<RefCell<StyledNode>> {
  let root_borrow = root.borrow();
  Arc::new(
    RefCell::new(
      StyledNode {
        node: root.clone(),
        specified_values: match root_borrow.node_type {
          NodeType::Element(ref elem) => specified_values(elem, stylesheet),
          _ => HashMap::new(),
        },
        children: root_borrow
          .children
          .iter()
          .map(|child| style_tree(child.clone(), stylesheet))
          .collect(),
      }
    )
  )
}

pub fn match_rules<'a>(elem: &ElementData, stylesheet: &'a CSSStylesheet) -> Vec<MatchedRule<'a>> {
  stylesheet
    .items
    .iter()
    .map(|item| match item {
      CSSStylesheetItem::QualifiedRule(CSSQualifiedRule::Selectors(rule)) => match_rule(elem, rule),
      _ => vec![],
    })
    .filter(|i| !i.is_empty())
    .flatten()
    .collect()
}

pub fn match_rule<'a>(
  elem: &ElementData,
  rule: &'a CSSQualifiedRuleSelectors,
) -> Vec<MatchedRule<'a>> {
  rule
    .selectors
    .items
    .iter()
    .filter(|s| match_selector(elem, s))
    .map(|s| (s.specificity(), rule))
    .collect()
}

pub fn match_selector(elem: &ElementData, selector: &CSSSelector) -> bool {
  use CSSSelectorItem::*;
  if selector.is_simple() {
    for item in &selector.items {
      let match_item = match item {
        Id { text: id } => match elem.id() {
          Some(elem_id) => elem_id == id,
          _ => false,
        },
        Class { text: class } => elem.classes().iter().any(|c| c == class),
        Tag { text: tag_name } => &elem.tag_name == tag_name,
        All => true,
        _ => false,
      };
      if !match_item {
        return false;
      }
    }
    true
  } else {
    false
  }
}

pub fn specified_values(elem: &ElementData, stylesheet: &CSSStylesheet) -> PropertyMap {
  let mut values = HashMap::new();
  let mut rules = match_rules(elem, stylesheet);
  rules.sort_by(|&(a, _), &(b, _)| a.cmp(&b));
  for (_, rule) in rules {
    for rule_item in &rule.declaration_list.items {
      if let CSSDeclarationListItem::Declaration(declaration) = rule_item {
        values.insert(declaration.ident.clone(), declaration.clone());
      }
    }
  }
  values
}
