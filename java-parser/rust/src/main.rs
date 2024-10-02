use jni::objects::{JClass, JObject, JString, JValue, JValueGen};
use jni::{AttachGuard, InitArgsBuilder, JNIEnv, JNIVersion, JavaVM};

fn main() {
    let jvm_args = InitArgsBuilder::new()
        .version(JNIVersion::V8)
        .option("-Xcheck:jni")
        .option("-Djava.class.path=../java/build/libs/parser-all.jar")
        .build()
        .unwrap();

    let jvm = JavaVM::new(jvm_args).unwrap();
    let mut guard = jvm.attach_current_thread().unwrap();

    let mut env = jvm.get_env().unwrap();
    let dummy_class = env.find_class("org/example/Parser").unwrap();

    hello(&mut guard);
    Java_org_example_Parser_rustFunc(env, dummy_class, &mut guard);
}

fn hello(guard: &mut AttachGuard) {
    let system = guard.find_class("java/lang/System").unwrap();
    let _print_stream = guard.find_class("java/io/PrintStream").unwrap();

    let out = guard
        .get_static_field(system, "out", "Ljava/io/PrintStream;")
        .unwrap();

    if let JValueGen::Object(out) = out {
        let message = guard.new_string("Hello World").unwrap();
        guard
            .call_method(
                out,
                "println",
                "(Ljava/lang/String;)V",
                &[JValue::Object(&message)],
            )
            .unwrap();
    }
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern "system" fn Java_org_example_Parser_rustFunc(
    mut env: JNIEnv,
    _class: JClass,
    guard: &mut AttachGuard,
) {
    let class = env
        .find_class("org/example/Parser")
        .expect("Failed to load the target class");

    let path: JObject = guard.new_string("..").unwrap().into();
    let path = JValueGen::Object(&path);
    let result = env.call_static_method(
        &class,
        "ls",
        "(Ljava/lang/String;)Ljava/lang/String;",
        &[path],
    );
    let result = result.map_err(|e| e.to_string()).unwrap();
    if let JValueGen::Object(o) = result {
        let java_string: JString = o.into();
        let rust_string: String = env.get_string(&java_string).unwrap().into();
        println!("ls result : {}", rust_string);
    }

    let file_name: JObject = guard.new_string("../Example.java").unwrap().into();
    let file_name = JValueGen::Object(&file_name);
    let class_name: JObject = guard.new_string("Example").unwrap().into();
    let class_name = JValueGen::Object(&class_name);
    let result = env.call_static_method(&class, "getClass", "(Ljava/lang/String;Ljava/lang/String;)Lcom/github/javaparser/ast/body/ClassOrInterfaceDeclaration;", &[file_name, class_name]);
    result.map_err(|e| e.to_string()).unwrap();
}
