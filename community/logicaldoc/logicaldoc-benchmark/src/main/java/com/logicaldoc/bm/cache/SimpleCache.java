package com.logicaldoc.bm.cache;

import java.io.Serializable;
import java.util.Collection;

public abstract interface SimpleCache<K extends Serializable, V>
{
  public abstract boolean contains(K paramK);

  public abstract Collection<K> getKeys();

  public abstract V get(K paramK);

  public abstract void put(K paramK, V paramV);

  public abstract void remove(K paramK);

  public abstract void clear();
}
