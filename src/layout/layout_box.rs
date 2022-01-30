use crate::{
  css::cssom::{CSSComponentValue, CSSLength},
  style::styled_node::StyledNode,
};
use std::fmt::Debug;
use std::sync::Arc;
use std::cell::RefCell;

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Rect {
  pub x: f32,
  pub y: f32,
  pub width: f32,
  pub height: f32,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Dimensions {
  pub content: Rect,
  pub padding: EdgeSizes,
  pub border: EdgeSizes,
  pub margin: EdgeSizes,
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct EdgeSizes {
  pub left: f32,
  pub right: f32,
  pub top: f32,
  pub bottom: f32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoxType {
  BlockNode(Arc<RefCell<StyledNode>>),
  InlineNode(Arc<RefCell<StyledNode>>),
  AnonymousBlock,
}

#[derive(Clone, PartialEq, Debug)]
pub struct LayoutBox {
  pub dimensions: Dimensions,
  pub box_type: BoxType,
  pub children: Vec<Arc<RefCell<LayoutBox>>>,
}

impl LayoutBox {
  pub fn new(box_type: BoxType) -> LayoutBox {
    LayoutBox {
      box_type,
      dimensions: Default::default(),
      children: vec![],
    }
  }

  pub fn append(&mut self, child: Arc<RefCell<LayoutBox>>) {
    self.children.push(child);
  }

  pub fn get_styled_node(&self) -> Option<Arc<RefCell<StyledNode>>> {
    match &self.box_type {
      BoxType::BlockNode(node) | BoxType::InlineNode(node) => Some(node.clone()),
      BoxType::AnonymousBlock => None,
    }
  }

  pub fn layout(&mut self, container_block: &Dimensions) {
    match self.box_type {
      BoxType::BlockNode(_) => self.layout_block(container_block),
      BoxType::InlineNode(_) | BoxType::AnonymousBlock => {}
    }
  }

  pub fn layout_block(&mut self, container_block: &Dimensions) {
    self.calculate_block_width(container_block);
    self.calculate_block_position(container_block);
    self.layout_block_children();
    self.calculate_block_height();
  }

  pub fn calculate_block_width(&mut self, container_block: &Dimensions) {
    if let Some(style_node) = self.get_styled_node() {
      let style = style_node.borrow();
      let auto = CSSLength::Auto;
      let mut width = style.declaration_nth_component_map_or_else(
        "width",
        0,
        CSSComponentValue::to_length,
        || auto.clone(),
      );

      let zero = CSSLength::Number { number: 0. };
      let mut margin_left = style.declaration_nth_components_map_or_else(
        &[
          ("margin", 0),
          ("margin-width", 0),
          ("margin-left", 0),
          ("margin-left-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let mut margin_right = style.declaration_nth_components_map_or_else(
        &[
          ("margin", 0),
          ("margin-width", 0),
          ("margin-right", 0),
          ("margin-right-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let border_left = style.declaration_nth_components_map_or_else(
        &[
          ("border", 0),
          ("border-width", 0),
          ("border-left", 0),
          ("border-left-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let border_right = style.declaration_nth_components_map_or_else(
        &[
          ("border", 0),
          ("border-width", 0),
          ("border-right", 0),
          ("border-right-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let padding_left = style.declaration_nth_components_map_or_else(
        &[
          ("padding", 0),
          ("padding-width", 0),
          ("padding-left", 0),
          ("padding-left-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let padding_right = style.declaration_nth_components_map_or_else(
        &[
          ("padding", 0),
          ("padding-width", 0),
          ("padding-right", 0),
          ("padding-right-width", 0),
        ],
        CSSComponentValue::to_length,
        || zero.clone(),
      );
      let total: f32 = [
        &margin_left,
        &margin_right,
        &border_left,
        &border_right,
        &padding_left,
        &padding_right,
        &width,
      ]
      .iter()
      .map(|f| f.to_px())
      .sum();

      if !width.is_auto() && total > container_block.content.width {
        if margin_left.is_auto() {
          margin_left = zero.clone();
        }
        if margin_right.is_auto() {
          margin_right = zero.clone();
        }
      }

      let underflow = container_block.content.width - total;

      match (
        width.is_auto(),
        margin_left.is_auto(),
        margin_right.is_auto(),
      ) {
        (false, false, false) => {
          margin_right = CSSLength::Number {
            number: margin_right.to_px() + underflow,
          }
        }
        (false, false, true) => margin_right = CSSLength::Number { number: underflow },
        (false, true, false) => margin_left = CSSLength::Number { number: underflow },
        (true, _, _) => {
          if margin_left.is_auto() {
            margin_left = zero.clone()
          }
          if margin_right.is_auto() {
            margin_right = zero.clone()
          }
          if underflow >= 0.0 {
            width = CSSLength::Number { number: underflow }
          } else {
            width = zero;
            margin_right = CSSLength::Number {
              number: underflow / 2.0,
            }
          }
        }
        (false, true, true) => {
          margin_left = CSSLength::Number {
            number: underflow / 2.0,
          };
          margin_right = CSSLength::Number {
            number: underflow / 2.0,
          };
        }
      }

      let d = &mut self.dimensions;
      d.content.width = width.to_px();

      d.padding.left = padding_left.to_px();
      d.padding.right = padding_right.to_px();

      d.border.left = border_left.to_px();
      d.border.right = border_right.to_px();

      d.margin.left = margin_left.to_px();
      d.margin.right = margin_right.to_px();
    }
  }

  pub fn calculate_block_position(&mut self, containing_block: &Dimensions) {
    if let Some(style_node) = self.get_styled_node() {
      let style = style_node.borrow();
      let d = &mut self.dimensions;
      let zero = CSSLength::Number { number: 0.0 };

      d.margin.top = style
        .declaration_nth_components_map_or_else(
          &[
            ("margin", 0),
            ("margin-width", 0),
            ("margin-top", 0),
            ("margin-top-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.margin.bottom = style
        .declaration_nth_components_map_or_else(
          &[
            ("margin", 0),
            ("margin-width", 0),
            ("margin-bottom", 0),
            ("margin-bottom-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.border.top = style
        .declaration_nth_components_map_or_else(
          &[
            ("border", 0),
            ("border-width", 0),
            ("border-top", 0),
            ("border-top-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.border.bottom = style
        .declaration_nth_components_map_or_else(
          &[
            ("border", 0),
            ("border-width", 0),
            ("border-bottom", 0),
            ("border-bottom-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.padding.top = style
        .declaration_nth_components_map_or_else(
          &[
            ("padding", 0),
            ("padding-width", 0),
            ("padding-top", 0),
            ("padding-top-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.border.bottom = style
        .declaration_nth_components_map_or_else(
          &[
            ("padding", 0),
            ("padding-width", 0),
            ("padding-bottom", 0),
            ("padding-bottom-width", 0),
          ],
          CSSComponentValue::to_length,
          || zero.clone(),
        )
        .to_px();
      d.content.x = containing_block.content.x + d.margin.left + d.border.left + d.padding.left;
      d.content.y = containing_block.content.height
        + containing_block.content.y
        + d.margin.top
        + d.border.top
        + d.padding.top;
    }
  }

  pub fn layout_block_children(&mut self) {
    let d = &mut self.dimensions;
    for child in &mut self.children {
      let mut child_borrow = child.borrow_mut();
      child_borrow.layout(d);
      d.content.height += child_borrow.dimensions.margin_box().height;
    }
  }

  pub fn calculate_block_height(&mut self) {
    if let Some(csslength) = self
      .get_styled_node()
      .and_then(|s| {
        let s_borrow = s.borrow();
        s_borrow.declaration_nth_component_map("width", 0usize, CSSComponentValue::to_length)
      })
    {
      if !csslength.is_auto() {
        self.dimensions.content.height = csslength.to_px();
      }
    }
  }

  pub fn get_inline_container(me: &Arc<RefCell<LayoutBox>>) -> Arc<RefCell<LayoutBox>> {
    let mut me_borrow = me.borrow_mut();
    match me_borrow.box_type {
      BoxType::InlineNode(_) | BoxType::AnonymousBlock => me.clone(),
      BoxType::BlockNode(_) => {
        let to_append = match me_borrow.children.last() {
          Some(last) => {
            let last_borrow = last.borrow();
            !matches!(&*last_borrow,
              LayoutBox {
                box_type: BoxType::AnonymousBlock,
                ..
              })
          }
          _ => true
        };
        if to_append {
          me_borrow.append(Arc::new(RefCell::new(LayoutBox::new(BoxType::AnonymousBlock))));
        }
        me_borrow.children.last().unwrap().clone()
      }
    }
  }
}

impl Rect {
  #[must_use]
  pub fn expaned_by(&self, edge: &EdgeSizes) -> Rect {
    Rect {
      x: self.x - edge.left,
      y: self.y - edge.top,
      width: self.width + edge.left + edge.right,
      height: self.height + edge.top + edge.bottom,
    }
  }
}

impl Dimensions {
  pub fn padding_box(&self) -> Rect {
    self.content.expaned_by(&self.padding)
  }

  pub fn border_box(&self) -> Rect {
    self.padding_box().expaned_by(&self.border)
  }

  pub fn margin_box(&self) -> Rect {
    self.border_box().expaned_by(&self.padding)
  }
}
