use flatbuffers::FlatBufferBuilder;
use flatcfe::compile;
use flatcfe::flatc;

fn main() {
    let mut args = std::env::args();
    if args.len() == 1 {
        println!("Usage: ./flatcfe [schema.fbs]+");
        return;
    }
    args.next(); // Skip program name.
    let filepaths: Vec<_> = args.collect();
    let mut fbb = FlatBufferBuilder::new();

    compile(&filepaths, &mut fbb);

    let buf = fbb.finished_data();
    let c = flatbuffers::get_root::<flatc::Compilation>(buf);
    println!("{:#?}", c);
}
