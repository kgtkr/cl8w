{
  const f = (n: number) => {
    let x = 1;
    return () => {
      x *= n;
      return x;
    }
  }

  const g = f(2);
  g();
  g();
  g();
}