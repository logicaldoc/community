package com.logicaldoc.util.cache;

import java.io.Serializable;
import java.util.Collection;

/**
 * A generic cache
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 *
 * @param <K> the key
 * @param <V> the value
 */
public interface Cache<K extends Serializable, V> {

	public boolean contains(K key);

	public Collection<K> getKeys();

	public V get(K key);

	public void put(K key, V value);

	public void remove(K key);

	public void clear();

	public void flush();

	public long getSize();
}