class Result<T, E> {
   readonly ok: bool
   readonly value?: T;
   readonly error?: E;

   private constructor(arg: {ok: true, value: T} | {ok: false, error: E}) {
      if (arg.ok) {
         this.ok = true;
         this.value = arg.v;
      } else {
         this.ok = false;
         this.error = arg.error
      }
   } 

   static ok<T, E = never>(value: T): Result<T, E> {
      return new Result<T, E>({ok: true, value});
   }

   static error<T = never, E = unknown>(error: E): Result<T, E> {
      return new Result<T, E>({ok: false, error});
   }

   andThen<U>(action: (T) => Result<U, E>): Result<U, E> {
      if (!this.ok) {
         return this;
      }
      return action(this.value);
   }   

   map<U>(f: (T) => U): Result<U, E> {
      if (!this.ok) {
         return this;
      }
      return new Result<T, U>(f(this.value));
   }
}
