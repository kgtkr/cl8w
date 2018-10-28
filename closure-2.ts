{
  interface F_Closure {
    n: number,
    x: number,
  }

  const f = (n: number): F_Closure => ({
    n: n,
    x: 1,
  });

  const f_closure = (cap: F_Closure) => {
    cap.x *= cap.n;
    return cap.x;
  };

  const g = f(2);
  f_closure(g);
  f_closure(g);
  f_closure(g);
}