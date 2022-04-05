class Never
  new create(a: Never) => a
  fun string(): String => "Impossible!"

primitive Logic
  fun always(a: Any, b: Any) => a
  fun never(a: Never): Never => Never(a)

primitive Left
primitive Right
type Either[A,B] is
    ( (Left, A)
    | (Right, B)
    )

primitive NeverFun
  // -- onlyLeft :  Either a Never -> Either a Never
  fun onlyLeft[A](a: Either[A, Never]): Either[A, Never] =>
    match a
    | (Left, let x: A) => (Left, x)
    | (Right, let n: Never) => (Right, Logic.never(n))
    end

  // -- onlyRight :  Either Never a -> Either Never a
  fun onlyRight[A](a: Either[Never, A]): Either[Never, A] =>
    match a
    | (Left, let n: Never) => (Left, Logic.never(n))
    | (Right, let x: A) => (Right, x)
    end

class Something[A]
  let a: A!

  new create(x: A) =>
    a = x

class Box
  var a: String iso

  new create(x: String iso) =>
    a = consume x

  fun ref update(x: String iso): String iso^ =>
    let b = a = consume x
    consume b

class Box2[A]
  var a: A
  var b: A

  new create(x: A^) =>
    a = x
    b = x

primitive Foo
  fun doit(s: String iso) =>
    None

actor Main
  new create(env: Env) =>
    doSomething(env)
    box1(env)
    box2(env)
    tupleTest(env)
    neverLeftTest(env)
    neverRightTest(env)
    speak(env)

    env.out.print("Hello, World!")

  be doSomething(env: Env) =>
    let a = Something[U8](42)
    let b = Something[String ref](recover ref String end)
    let c = Something[String iso](recover iso String end)

  be box1(env: Env) =>
    let d = Box(recover iso String end)
    let e = d.update(recover iso String end)
    Foo.doit(consume e)
    // env.out.print(e)

  be box2(env: Env) =>
    let f = Box2[String ref](recover ref String end)
    let g = Box2[String val](recover val String end)

  be tupleTest(env: Env) =>
    let h: (String, U8, F64) = ("Hello", 42, 3.1514)
    let i = ("Hello", "there,", "World!")
    env.out.print(i._1 + " " + i._2 + " " + i._3)

  be neverLeftTest(env: Env) =>
    let j: Either[Never,I8] = NeverFun.onlyRight[I8]((Right, 16))
    let k: String =
      match j
      | (Left, let x: Never) => "You'll never see this!"
      | (Right, let x: I8) => x.string()
      end
    env.out.print(k)

  be neverRightTest(env: Env) =>
    let j: Either[I8,Never] = NeverFun.onlyLeft[I8]((Left, 16))
    let k: String =
      match j
      | (Left, let x: I8) => x.string()
      | (Right, let x: Never) => "You'll never see this!"
      end
    env.out.print(k)

  be speak(env: Env) =>
    env.out.print("Hello, Behaviour!")
