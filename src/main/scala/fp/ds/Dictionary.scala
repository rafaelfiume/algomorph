package fp.ds

object Dictionary {

  type Dictionary[A] = BinTree[(String, A)]

  def empty[A]: Dictionary[A] = Leaf

  def insert[A](d: Dictionary[A], key: String, value: A): Dictionary[A] = d match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, _), _, _) if k == key => sys.error(s"key <$key> already exists in the dictionary")
    case Branch((k, v), l, r) if k > key => Branch((k, v), insert(l, key, value), r)
    case Branch((k, v), l, r) if k < key => Branch((k, v), l, insert(r, key, value))
  }

  def update[A](d: Dictionary[A], key: String, value: A): Dictionary[A] = d match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, v), l, r) if k == key => Branch((k, v), l, r)
    case Branch((k, v), l, r) if k > key => Branch((k, v), update(l, key, value), r)
    case Branch((k, v), l, r) if k < key => Branch((k, v), l, update(r, key, value))
  }

  def search[A](d: Dictionary[A], key: String): Option[A] = d match {
    case Leaf => None
    case Branch((k, v), _, _) if k == key => Some(v)
    case Branch((k, _), l, _) if k > key => search(l, key)
    case Branch((k, _), _, r) if k < key => search(r, key)
  }

}
