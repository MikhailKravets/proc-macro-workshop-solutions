use seq::seq;


fn main() {
    seq!(N in 1..3 {
        fn fun~N~a() -> usize {
            N * 2
        }
    });

    seq!(N in 1..3 {
        #[derive(Debug)]
        enum Processor {
            #(
                Cp~N,
            )*
        }
    });

}
