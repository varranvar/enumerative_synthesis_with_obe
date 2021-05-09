#![feature(wrapping_int_impl)]

use std::num::Wrapping;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use std::collections::HashMap;
use std::time::Instant;
use csv::Writer;


type Number = i8;
const MAX: Number = std::i8::MAX;
const MIN: Number = std::i8::MIN;

const COUNT: usize = 256;

type Value = Wrapping<Number>;
type Example  = (Value, Value);
type Examples = [Example; COUNT];
type Specification = fn(Value) -> Value;

const PROBLEMS: [Specification; 9] = [
    problem1,
    problem2,
    problem3,
    problem4,
    problem5,
    problem6,
    problem7,
    problem8,
    problem13,  // Easy height-3 problem
    //problem17 // Hard height-3 problem
];

#[derive(Debug, Clone)]
enum Term {
    One,
    Zero,
    Max,
    Min,
    Variable,
    UnOp(UnOp, Box<Term>),
    BiOp(BiOp, Box<Term>, Box<Term>)
}

#[derive(Debug, Clone, EnumIter)]
enum BiOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Xor,
    RightShift,
    LeftShift,
    //GT,
    //GE,
    //LT,
    //LE,
}

#[derive(Debug, Clone, EnumIter)]
enum UnOp {
    Not,
}

impl Term {
    fn evaluate(&self, input: &Value) -> Value {
        match self {
            Term::Zero => Wrapping(0),
            Term::One => Wrapping(1),
            Term::Max => Wrapping(MAX),
            Term::Min => Wrapping(MIN),
            Term::Variable => *input,
            Term::UnOp(op, x_unevaluated) => {
                let x = x_unevaluated.evaluate(input);
                match op {
                    UnOp::Not => !x,
                }
            }
            Term::BiOp(op, x_unevaluated, y_unevaluated) => {
                let x = x_unevaluated.evaluate(input);
                let y = y_unevaluated.evaluate(input);
                match op {
                    BiOp::Add => x + y,
                    BiOp::Subtract => x - y,
                    BiOp::Multiply => x * y,
                    BiOp::Divide => if y == Wrapping(0) {x} else {x / y},
                    BiOp::And => x & y,
                    BiOp::Or => x | y,
                    BiOp::Xor => x ^ y,
                    BiOp::RightShift => x.rotate_right(y.0 as u32),
                    BiOp::LeftShift => x.rotate_left(y.0 as u32),
                    //BiOp::GT => if x > y {Wrapping(1)} else {Wrapping(0)},
                    //BiOp::LT => if x < y {Wrapping(1)} else {Wrapping(0)},
                    //BiOp::GE => if x >= y {Wrapping(1)} else {Wrapping(0)},
                    //BiOp::LE => if x <= y {Wrapping(1)} else {Wrapping(0)},
                }
            }
        }
    }
    
    fn size(&self) -> usize {
        match self {
            Term::UnOp(_, x) => 1 + x.size(),
            Term::BiOp(_, x, y) => 1 + x.size() + y.size(),
            _ => 1,
        }
    }
}

use std::fmt;
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Zero => write!(f, "{}", 0),
            Term::One => write!(f, "{}", 1),
            Term::Max => write!(f, "{}", MAX),
            Term::Min => write!(f, "{}", MIN),
            Term::Variable => write!(f, "x"),
            Term::UnOp(op, x) => {
                write!(f, "({} {})", 
                    match op {
                        UnOp::Not => "!",
                    },
                    x
                )
            },
            Term::BiOp(op, x, y) => {
                write!(f, "({} {} {})", 
                    match op {
                        BiOp::Add => "+",
                        BiOp::Subtract => "-",
                        BiOp::Multiply => "*",
                        BiOp::Divide => "/",
                        BiOp::And => "&",
                        BiOp::Or => "|",
                        BiOp::Xor => "^",
                        BiOp::RightShift => ">>",
                        BiOp::LeftShift => "<<",
                        //BiOp::GT => ">",
                        //BiOp::LT => "<",
                        //BiOp::GE => ">=",
                        //BiOp::LE => "<=",
                    },
                    x,
                    y
                )
            }
        }
    }
}

#[derive(Debug)]
struct PartialSolution {
    program: Term,
    reward: usize,
}

type Bank = Vec<PartialSolution>;

fn synthesize(examples: &Examples) -> (Term, usize) {
    let mut bank = Vec::new();
    let mut count = 1;

    // Verify zero arity programs
    for program in [Term::Variable, Term::Zero, Term::One, Term::Max, Term::Min].iter() {
        if verify(&mut bank, examples, &program) {
            return ((*program).clone(), count);
        }
        count += 1;
    }

    // Generate and verify programs
    let mut height = 0;
    loop {
        println!("HEIGHT: {}, BANK SIZE: {}", height, bank.len());

        bank.sort_unstable_by(|x, y| y.reward.cmp(&x.reward));    

        let len = bank.len();
        for x in 0..len {
            for operation in UnOp::iter() {
                let program = Term::UnOp(operation, Box::new(bank[x].program.clone()));
                if verify(&mut bank, examples, &program) {
                    return (program, count);
                }
                count += 1;
            }

            for y in 0..len {
                for operation in BiOp::iter() {
                    let program = Term::BiOp(operation, Box::new(bank[x].program.clone()), Box::new(bank[y].program.clone()));
                    if verify(&mut bank, examples, &program) {
                        return (program, count);
                    }
                    count += 1;
                }
            }
        }
        
        height += 1;  
    } 

    fn verify(bank: &mut Bank, examples: &Examples, program: &Term) -> bool {
        let mut reward = 0;
        for (inp, out) in examples  {
            if program.evaluate(&inp) == *out {
                reward += 1;
            }            
        }

        if reward == COUNT {
            true
        } else {
            bank.push(PartialSolution{program: (*program).clone(), reward});
            false
        }

    }
}




type Map = HashMap<[Value; COUNT], usize>;

fn synthesize_equivalence_reduction(examples: &Examples) -> (Term, usize) {
    let mut bank= Vec::new();
    let mut map: Map= HashMap::new();
    let mut count = 1;

    // Verify zero arity programs
    for program in [Term::Variable, Term::Zero, Term::One, Term::Max, Term::Min].iter() {
        if verify(&mut bank, &mut map, examples, &program) {
            return ((*program).clone(), count);
        }
    }

    // Generate and verify programs
    let mut height = 0;
    loop {
        println!("HEIGHT: {}, BANK SIZE: {}", height, bank.len());

        bank.sort_unstable_by(|x, y| y.reward.cmp(&x.reward));  

        let len = bank.len();
        for x in 0..len {
            for operation in UnOp::iter() {
                let program =  Term::UnOp(operation, Box::new(bank[x].program.clone()));
    
                if verify(&mut bank, &mut map, examples, &program) {
                    return (program, count);
                }
                count += 1;
            }

            for y in 0..len {
                for operation in BiOp::iter() {
                    let program = Term::BiOp(operation, Box::new(bank[x].program.clone()), Box::new(bank[y].program.clone()));

                    if verify(&mut bank, &mut map, examples, &program) {
                        return (program, count);
                    }
                    count += 1;
                }
            }
        }
        height += 1;  
    } 

    fn verify(bank: &mut Bank, map: &mut Map, examples: &Examples, program: &Term) -> bool {
        // Evaluate program on examples
        let mut outputs = [Wrapping(0); COUNT];
        let mut reward = 0;
        for i in 0..COUNT  {
            let output = program.evaluate(&examples[i].0);
            outputs[i] = output;
            if output == examples[i].1 {
                reward += 1;
            }
        }

        // Return program if it satisfied the specification
        if reward == COUNT {
            return true;
        }


        // Reduce observationally equivalent programs
        let partial_solution = PartialSolution {program: program.clone(), reward};
        if let Some(&index) = map.get(&outputs) {
            if program.size() < bank[index].program.size() {
                bank[index] = partial_solution;
            }
        } else {
            map.insert(outputs, bank.len());
            bank.push(partial_solution);
        }

        false
    }
}


fn main() {
    let mut writer = Writer::from_path("data.csv").unwrap();
    writer.write_record(&["Problem", "Control Synthesis Time (μs)", "Control Iteration Count", "Test Synthesis Time (μs)", "Test Iteration Count"]).unwrap();

    for (p, problem) in PROBLEMS.iter().enumerate() {
        let examples= examples(problem);
        //println!("{:?}", examples);

        println!("========PROBLEM {}========", p + 1);

        // Synthesize and time control algorithm
        let now = Instant::now();
        let (control_program, control_count) = synthesize(&examples);
        let control_time = now.elapsed().as_micros();
        println!("CONTROL SOLUTION: {}", control_program);
        println!("CONTROL COUNT: {}", control_count);
        println!("CONTROL ELAPSED (μs): {}\n", control_time);


        // Synthesize and time test algorithm
        let now = Instant::now();
        let (test_program, test_count) = synthesize_equivalence_reduction(&examples);
        let test_time = now.elapsed().as_micros();
        println!("TEST SOLUTION: {}", test_program);
        println!("TEST COUNT: {}", test_count);
        println!("TEST ELAPSED (μs): {}\n", test_time);
        
        writer.write_record(&[format!("{}", p + 1),  format!("{}", control_time), format!("{}", control_count), format!("{}", test_time), format!("{}", test_count)]).unwrap();
    }
}

pub fn examples(specification: &fn(Wrapping<Number>) -> Wrapping<Number>) -> [(Wrapping<Number>, Wrapping<Number>); COUNT] {
    let mut examples= [(Wrapping(0), Wrapping(0)); COUNT];
    for (i, inp) in (MIN..=MAX).enumerate() {
        examples[i] = (Wrapping(inp), specification(Wrapping(inp)));
    }
    examples
}


//////////////
// PROBLEMS //
//////////////

// P1 ~ Turn off rightmost 1 bit
fn problem1(x: Value) -> Value {
    x & (x - Wrapping(1))
}

// P2 ~ Test whether an unsigned integer is of form 2^(n-1)
fn problem2(x: Value) -> Value {
    x & (x + Wrapping(1))
}

// P3 ~ Isolate rightmost bit
fn problem3(x: Value) -> Value {
    x & -x
}

// P4 ~ Form a mask that identifies the rightmost 1 bit and trailing 0s
fn problem4(x: Value) -> Value {
    x ^ (x - Wrapping(1))
}

// P5 ~ Right propagate rightmost 1 bit
fn problem5(x: Value) -> Value {
    x | (x - Wrapping(1))
}

// / P6 ~ Turn on rightmost 0 bit
fn problem6(x: Value) -> Value {
    x | (x + Wrapping(1))
}

// P7 ~ Isolate rightmost 0 bit
fn problem7(x: Value) -> Value {
    (!x) & (x + Wrapping(1))
}

// P8 ~ Form a mask that identifies trailing 0s
fn problem8(x: Value) -> Value {
    (x - Wrapping(1)) & (!x)
}

// P13 ~ Sign function
fn problem13(x: Value) -> Value {
    let o1 = x.rotate_right(MAX as u32);
    let o2 = -x;
    let o3 = o2.rotate_right(MAX as u32);
    return o1 | o3;
}


/*
// P17 ~ Turn-off the rightmost contiguous string of 1 bits
fn problem17(x: Value) -> Value {
    let o1 = x - Wrapping(1);
    let o2 = x | o1;
    let o3 = o2 + Wrapping(1);
    return o3 & x;
}
*/