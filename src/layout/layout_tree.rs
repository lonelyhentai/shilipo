use crate::css::cssom::{ CSSDisplay };
use crate::style::styled_node::{ StyledNode };
use crate::layout::layout_box::{ LayoutBox, Dimensions, BoxType };
use std::cell::RefCell;
use std::sync::Arc;

pub fn layout_tree (node: Arc<RefCell<StyledNode>>, mut containing_block: Dimensions) -> Arc<RefCell<LayoutBox>> {
  containing_block.content.height = 0f32;
  let root_box = build_layout_tree_recursive (node);

  root_box.borrow_mut().layout(&containing_block);
  root_box
}

fn build_layout_tree_recursive(style_node: Arc<RefCell<StyledNode>>) -> Arc<RefCell<LayoutBox>> {
  let root = Arc::new(
    RefCell::new(
      LayoutBox::new(match style_node.borrow().display() {
        CSSDisplay::Block => BoxType::BlockNode(style_node.clone()),
        CSSDisplay::Inline | CSSDisplay::Replace => BoxType::InlineNode(style_node.clone()),
        CSSDisplay::None => panic!("root node has display: none.")
      })
    )
  );

  for child in &style_node.borrow().children {
    let child_borrow = child.borrow();
    match child_borrow.display() {
      CSSDisplay::Block => root.borrow_mut().append(
        build_layout_tree_recursive(child.clone())
      ),
      CSSDisplay::Inline | CSSDisplay::Replace => {
        let container = LayoutBox::get_inline_container(&root);
        container.borrow_mut().append(
          build_layout_tree_recursive(child.clone())
        );
      },
      CSSDisplay::None => {}
    }
  }
  root
}