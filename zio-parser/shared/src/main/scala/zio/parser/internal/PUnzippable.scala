package zio.parser.internal

// Based on zio.Unzippable, extended with UnzippableLeftIdentityAny, renamed to PUnzippable to avoid name collision

trait PUnzippable[A, B] {
  type In
  def unzip(in: In): (A, B)
}

object PUnzippable extends PUnzippableLowPriority0 {

  type In[A, B, C] = PUnzippable[A, B] { type In = C }

  implicit def UnzippableLeftIdentityAny[A]: PUnzippable.In[Any, A, A] =
    new PUnzippable[Any, A] {
      type In = A
      def unzip(in: A): (Any, A) =
        ((), in)
    }
}

trait PUnzippableLowPriority0 extends PUnzippableLowPriority1 {

  implicit def UnzippableLeftIdentity[A]: PUnzippable.In[Unit, A, A] =
    new PUnzippable[Unit, A] {
      type In = A
      def unzip(in: A): (Unit, A) =
        ((), in)
    }
}

trait PUnzippableLowPriority1 extends PUnzippableLowPriority2 {

  implicit def UnzippableRightIdentity[A]: PUnzippable.In[A, Unit, A] =
    new PUnzippable[A, Unit] {
      type In = A
      def unzip(in: A): (A, Unit) =
        (in, ())
    }
}

trait PUnzippableLowPriority2 extends PUnzippableLowPriority3 {

  implicit def Unzippable3[A, B, Z]: PUnzippable.In[(A, B), Z, (A, B, Z)] =
    new PUnzippable[(A, B), Z] {
      type In = (A, B, Z)
      def unzip(in: (A, B, Z)): ((A, B), Z) =
        ((in._1, in._2), in._3)
    }

  implicit def Unzippable4[A, B, C, Z]: PUnzippable.In[(A, B, C), Z, (A, B, C, Z)] =
    new PUnzippable[(A, B, C), Z] {
      type In = (A, B, C, Z)
      def unzip(in: (A, B, C, Z)): ((A, B, C), Z) =
        ((in._1, in._2, in._3), in._4)
    }

  implicit def Unzippable5[A, B, C, D, Z]: PUnzippable.In[(A, B, C, D), Z, (A, B, C, D, Z)] =
    new PUnzippable[(A, B, C, D), Z] {
      type In = (A, B, C, D, Z)
      def unzip(in: (A, B, C, D, Z)): ((A, B, C, D), Z) =
        ((in._1, in._2, in._3, in._4), in._5)
    }

  implicit def Unzippable6[A, B, C, D, E, Z]: PUnzippable.In[(A, B, C, D, E), Z, (A, B, C, D, E, Z)] =
    new PUnzippable[(A, B, C, D, E), Z] {
      type In = (A, B, C, D, E, Z)
      def unzip(in: (A, B, C, D, E, Z)): ((A, B, C, D, E), Z) =
        ((in._1, in._2, in._3, in._4, in._5), in._6)
    }

  implicit def Unzippable7[A, B, C, D, E, F, Z]: PUnzippable.In[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)] =
    new PUnzippable[(A, B, C, D, E, F), Z] {
      type In = (A, B, C, D, E, F, Z)
      def unzip(in: (A, B, C, D, E, F, Z)): ((A, B, C, D, E, F), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6), in._7)
    }

  implicit def Unzippable8[A, B, C, D, E, F, G, Z]: PUnzippable.In[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G), Z] {
      type In = (A, B, C, D, E, F, G, Z)
      def unzip(in: (A, B, C, D, E, F, G, Z)): ((A, B, C, D, E, F, G), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7), in._8)
    }

  implicit def Unzippable9[A, B, C, D, E, F, G, H, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H), Z] {
      type In = (A, B, C, D, E, F, G, H, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, Z)): ((A, B, C, D, E, F, G, H), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8), in._9)
    }

  implicit def Unzippable10[A, B, C, D, E, F, G, H, I, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I), Z] {
      type In = (A, B, C, D, E, F, G, H, I, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, Z)): ((A, B, C, D, E, F, G, H, I), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9), in._10)
    }

  implicit def Unzippable11[A, B, C, D, E, F, G, H, I, J, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, Z)): ((A, B, C, D, E, F, G, H, I, J), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10), in._11)
    }

  implicit def Unzippable12[A, B, C, D, E, F, G, H, I, J, K, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, Z)): ((A, B, C, D, E, F, G, H, I, J, K), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11), in._12)
    }

  implicit def Unzippable13[A, B, C, D, E, F, G, H, I, J, K, L, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12), in._13)
    }

  implicit def Unzippable14[A, B, C, D, E, F, G, H, I, J, K, L, M, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L, M), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12, in._13), in._14)
    }

  implicit def Unzippable15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z]
      : PUnzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z) =
        (
          (in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12, in._13, in._14),
          in._15
        )
    }

  implicit def Unzippable16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15
          ),
          in._16
        )
    }

  implicit def Unzippable17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16
          ),
          in._17
        )
    }

  implicit def Unzippable18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17
          ),
          in._18
        )
    }

  implicit def Unzippable19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18
          ),
          in._19
        )
    }

  implicit def Unzippable20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19
          ),
          in._20
        )
    }

  implicit def Unzippable21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19,
            in._20
          ),
          in._21
        )
    }

  implicit def Unzippable22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z]: PUnzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
  ] =
    new PUnzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      def unzip(
          in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19,
            in._20,
            in._21
          ),
          in._22
        )
    }
}

trait PUnzippableLowPriority3 {

  implicit def Unzippable2[A, B]: PUnzippable.In[A, B, (A, B)] =
    new PUnzippable[A, B] {
      type In = (A, B)
      def unzip(in: (A, B)): (A, B) =
        (in._1, in._2)
    }
}
