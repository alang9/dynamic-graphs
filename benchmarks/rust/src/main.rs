extern crate criterion;
extern crate outils;

use outils::prelude::*;
use std::collections::HashMap;

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd)]
enum Instruction<T>{
    Reset(<usize>)
    InsertVertex(VertexIndex<T>),
    InsertEdge(VertexIndex<T>,VertexIndex<T>),
    DeleteVertex(VertexIndex<T>),
    DeleteEdge(VertexIndex<T>,VertexIndex<T>),
    Connected(VertexIndex<T>,VertexIndex<T>,bool)
}
impl<T> Instruction<T>{
    fn is_insert_vertex(&self) -> Option<T>
    where T:Copy
    {
        match *self {
            InsertVertex(VertexIndex(x)) => Some(x),
            _ => None
        }
    }
}
use Instruction::*;

fn run_program(program:&[Instruction<usize>]) {
    let size = program.iter().filter_map(|i|i.is_insert_vertex()).max().unwrap() + 1;
    let mut graph: DynamicGraph<EmptyWeight> = DynamicGraph::new(size, 3);
    let mut map: HashMap<(VertexIndex<usize>,VertexIndex<usize>),Edge<usize>> = HashMap::new();
    for i in program {
        match *i {
            Reset(x) => {
                let mut graph: DynamicGraph<EmptyWeight> = DynamicGraph::new(size, 3);
            },
            InsertVertex(x) => {},
            InsertEdge(x,y) => {
                let xy = graph.insert_edge(x, y).expect("No reason to fail here");
                map.insert((x,y), xy);
            }
            DeleteVertex(x) => {},
            DeleteEdge(x,y) => {
                let edge = map.get(&(x,y)).expect("shouldn't happen");
                graph.delete_edge(*edge);
            }
            Connected(x,y,b) => {
                assert_eq!(graph.is_connected(x,y),b)
            }

            _ => {},
        }
    }
}

fn main() {
    let mut map: HashMap<(VertexIndex<u64>,VertexIndex<u64>),Edge<u64>> = HashMap::new();
    // Construct a unweighted dynamic graph with a fixed number of 10 vertices with an expected
    // degree (i.e. number of adjacent edges) of 3.

    run_program(&[InsertVertex(VertexIndex(0)),
                  InsertVertex(VertexIndex(1)),
                  InsertEdge(VertexIndex(0),
                             VertexIndex(1)),
                  Connected(VertexIndex(0),VertexIndex(1),false)]);

    // let mut graph: DynamicGraph<EmptyWeight> = DynamicGraph::new(10, 3);
    // let a = VertexIndex(0);
    // let b = VertexIndex(1);
    // let c = VertexIndex(2);
    // let d = VertexIndex(3);

    // // Create a cycle from a to d.
    // println!("{:#?}",graph.edges().collect::<Vec<_>>());
    // let ab = graph.insert_edge(a, b).expect("No reason to fail here");
    // println!("{:#?}",graph.edges().collect::<Vec<_>>());
    // let ab = graph.insert_edge(a, b).expect("No reason to fail here");
    // println!("{:#?}",graph.edges().collect::<Vec<_>>());
    // let bc = graph.insert_edge(b, c).expect("No reason to fail here");
    // let cd = graph.insert_edge(c, d).expect("No reason to fail here");
    // let da = graph.insert_edge(d, a).expect("No reason to fail here");

    // // a and c should be connected:
    // assert!(graph.is_connected(a, c));

    // graph.delete_edge(bc);

    // // a and c should still be connected:
    // assert!(graph.is_connected(a, c));

    // graph.delete_edge(da);

    // // NOW a and c should not be connected anymore:
    // assert!(!graph.is_connected(a, c));

    // println!("Hello, world!");
}
