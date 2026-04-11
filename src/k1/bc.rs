use crate::ir::IrUnit;
use crate::typer::TypedProgram;


pub struct BcUnit {
    instrs: Vec<BcInstr>,
}

pub fn lower_unit(k1: &mut TypedProgram, unit: IrUnit) -> BcUnit {

    let mut instrs = vec![];
    for block in k1.ir.mem.getn(unit.blocks) {

    }

}
