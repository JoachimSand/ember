use std::collections::HashMap;
use std::collections::HashSet;

use crate::ir::*;

pub fn get_dom_frontiers(
    dom_frontiers: Vec<Vec<BlockID>>,
    blocks: Vec<BasicBlockData>,
    cur_block: BlockID,
) {
}

// pub fn get_idom_tree(, blocks: Vec<BasicBlockData>, cur_block: BlockID) {

// }

fn post_order_traversal(
    y: BlockID,
    blocks: &Vec<BasicBlockData>,
    traversed: &mut HashSet<BlockID>,
    order: &mut Vec<BlockID>,
    order_set: &mut HashMap<BlockID, usize>,
) {
    traversed.insert(y);
    if let Some(term) = blocks[y].terminator {
        match term {
            Terminator::Brcond {
                if_true_dst,
                if_false_dst,
                ..
            } => {
                if traversed.contains(&if_true_dst) == false {
                    post_order_traversal(if_true_dst, blocks, traversed, order, order_set);
                }
                if traversed.contains(&if_false_dst) == false {
                    post_order_traversal(if_false_dst, blocks, traversed, order, order_set);
                }
            }
            Terminator::Br { dst } => {
                if traversed.contains(&dst) == false {
                    post_order_traversal(dst, blocks, traversed, order, order_set);
                }
            }
        }
    }

    order.push(y);
    order_set.insert(y, order.len() - 1);
}

// Promotes certain memory references to registers. Specfically, this pass targets
// memory locations allocated by alloca.
pub fn mem_to_reg(blocks: Vec<BasicBlockData>) {
    // Step 1: Determine the predecessor of each block.
    let mut preds: Vec<Vec<BlockID>> = Vec::with_capacity(blocks.len());
    let mut dom_frontiers: Vec<Vec<BlockID>> = Vec::with_capacity(blocks.len());
    for _ in 0..blocks.len() {
        preds.push(Vec::new());
        dom_frontiers.push(Vec::new());
    }

    for (i, block) in blocks.iter().enumerate() {
        if let Some(term) = block.terminator {
            match term {
                Terminator::Br { dst } => {
                    preds[dst].push(i);
                }
                Terminator::Brcond {
                    if_true_dst,
                    if_false_dst,
                    ..
                } => {
                    preds[if_true_dst].push(i);
                    preds[if_false_dst].push(i);
                }
            }
        }
    }

    for (i, pred) in preds.iter().enumerate() {
        println!("Preds for BB{i}: {pred:#?}");
    }

    // Step 2: Establish dominators.
    let mut dominators: Vec<Option<BlockID>> = Vec::with_capacity(blocks.len());
    // let all_nodes: HashSet<BlockID> = HashSet::from_iter((0..blocks.len()).into_iter());
    for _ in 0..blocks.len() {
        dominators.push(None);
    }

    dominators[START_BB] = Some(START_BB);

    // TODO: Is post-order needed?
    // For simple graphs not needed.
    // For the given graph, determine the post-order traversal
    let mut traversal_order = Vec::<BlockID>::new();
    // .. and the mapping from block id to position in this post-order traversal.
    let mut traversal_set = HashMap::<BlockID, usize>::new();
    post_order_traversal(
        START_BB,
        &blocks,
        &mut HashSet::new(),
        &mut traversal_order,
        &mut traversal_set,
    );
    println!("Traversal set: {traversal_set:?}");

    // Remove the root node from the traversal
    traversal_order.pop();
    println!("traversal: {traversal_order:?}");

    // Approach is based on paper "A Simple, Fast Dominance Algorithm" by Cooper et. al
    let mut changed = true;
    while changed {
        changed = false;

        let intersect = |b1: usize, b2: usize, dominators: &Vec<Option<BlockID>>| {
            let mut finger1 = b1;
            let mut finger2 = b2;

            while finger1 != finger2 {
                while traversal_set[&finger1] < traversal_set[&finger2] {
                    finger1 = dominators[finger1].unwrap();
                }

                while traversal_set[&finger2] < traversal_set[&finger1] {
                    finger2 = dominators[finger2].unwrap();
                }
            }
            return finger1;
        };

        // reverse post order traversal with exception of start bb
        for b in &traversal_order {
            let b = *b;
            let mut new_idom = preds[b][0];
            // iterate over all other preds of b
            for p in 1..preds[b].len() {
                let pred = preds[b][p];
                if dominators[pred].is_some() {
                    new_idom = intersect(pred, new_idom, &dominators);
                }
            }
            if dominators[b] != Some(new_idom) {
                dominators[b] = Some(new_idom);
                changed = true;
            }
        }
    }

    println!("Immediate dominators:");
    for (i, doms) in dominators.iter().enumerate() {
        println!("BB{i} has {doms:?}");
    }

    // Step 3: Determine the dominance frontiers for each block.
    let mut dominator_frontiers: Vec<HashSet<BlockID>> = Vec::with_capacity(blocks.len());
    for _ in 0..blocks.len() {
        dominator_frontiers.push(HashSet::new());
    }

    for b in 0..blocks.len() {
        if preds[b].len() >= 2 {
            println!("preds of {b} {:?}", preds[b]);
            for pred in &preds[b] {
                let mut runner = *pred;
                println!("runner {runner} b {b}");
                while runner != dominators[b].unwrap() {
                    if runner == 0 {
                        break;
                    } else {
                        dominator_frontiers[runner].insert(b);
                        runner = dominators[runner].unwrap();
                    }
                }
            }
        }
    }

    println!("Dominance frontiers:");
    for (i, df) in dominator_frontiers.iter().enumerate() {
        println!("BB{i} has dominance frontier {df:?}");
    }
}
