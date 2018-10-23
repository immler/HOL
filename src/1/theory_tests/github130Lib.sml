structure github130Lib =
struct

open HolKernel
val _ = Feedback.WARNINGs_as_ERRs := true;
val _ = Globals.print_thy_loads := true;

val ghdata = ref @{position} [] : thm list ref @{position}
fun add_ghdata (_, th) = (ghdata := th :: !ghdata)

val {export = export_gh130, dest, mk} = ThmSetData.new_exporter "gh130" (K (List.app add_ghdata))

end
