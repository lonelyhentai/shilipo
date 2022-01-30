use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

pub type AttrMap = HashMap<String, String>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElementData {
  pub tag_name: String,
  pub attributes: AttrMap,
}

impl ElementData {
  pub fn id(&self) -> Option<&String> {
    self.attributes.get("id")
  }

  pub fn classes(&self) -> HashSet<&str> {
    match self.attributes.get("class") {
      Some(classlist) => classlist.split(' ').collect(),
      None => HashSet::new(),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TextData {
  pub text: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CDataData {
  pub text: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommentData {
  pub text: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DocTypeData {
  pub legacy_compact: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NodeType {
  Element(ElementData),
  Text(TextData),
  CData(CDataData),
  Comment(CommentData),
  DocType(DocTypeData),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Node {
  pub node_type: NodeType,
  pub children: Vec<Arc<RefCell<Node>>>,
}

impl Node {
  pub fn text<S: Into<String>>(text: S) -> Self {
    Self {
      children: vec![],
      node_type: NodeType::Text(TextData { text: text.into() }),
    }
  }

  pub fn elem<S: Into<String>>(tag_name: S, attrs: AttrMap, children: Vec<Node>) -> Self {
    Self {
      children: children
        .into_iter()
        .map(|n| Arc::new(RefCell::new(n)))
        .collect(),
      node_type: NodeType::Element(ElementData {
        tag_name: tag_name.into(),
        attributes: attrs,
      }),
    }
  }

  pub fn append_node(&mut self, node: Node) {
    self.append(Arc::new(RefCell::new(node)));
  }

  pub fn append (&mut self, node: Arc<RefCell<Node>>) {
    self.children.push(node);
  }

  pub fn doctype(legacy_compact: bool) -> Self {
    Self {
      children: vec![],
      node_type: NodeType::DocType(DocTypeData { legacy_compact }),
    }
  }

  pub fn comment<S: Into<String>>(text: S) -> Self {
    Self {
      children: vec![],
      node_type: NodeType::Comment(CommentData { text: text.into() }),
    }
  }

  pub fn cdata<S: Into<String>>(text: S) -> Self {
    Self {
      children: vec![],
      node_type: NodeType::CData(CDataData { text: text.into() }),
    }
  }
}
