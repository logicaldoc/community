package com.logicaldoc.util.cache;

import java.io.Serializable;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Collection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheException;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

/**
 * A thin adapter for <b>Ehcache</b> support.
 * <p>
 * Thread-safety is taken care of by the underlying <b>Ehcache</b> instance.
 * <p>
 * see org.springframework.cache.ehcache.EhCacheFactoryBean <br>
 * see org.springframework.cache.ehcache.EhCacheManagerFactoryBean
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 5.0
 *
 * @param <K> The object identifier
 * @param <V> The object instance
 */
public class EhCache<K extends Serializable, V extends Object> implements com.logicaldoc.util.cache.Cache<K, V> {
	
	private static Logger log = LoggerFactory.getLogger(EhCache.class);

	private net.sf.ehcache.Cache cache;

	private static CacheManager manager;

	public EhCache(Cache cache) {
		super();
		this.cache = cache;
	}

	public boolean contains(K key) {
		try {
			return (cache.get(key) != null);
		} catch (CacheException e) {
			throw new CacheException("contains failed", e);
		}
	}

	@SuppressWarnings("unchecked")
	public Collection<K> getKeys() {
		return cache.getKeys();
	}

	@SuppressWarnings("unchecked")
	public V get(K key) {
		try {
			Element element = cache.get(key);
			if (element != null) {
				return (V) element.getObjectValue();
			} else {
				return null;
			}
		} catch (IllegalStateException ie) {
			throw new CacheException("Failed to get from EhCache as state invalid: \n" + "  state: " + cache.getStatus()
					+ "\n" + "   key: " + key, ie);
		} catch (CacheException e) {
			throw new CacheException("Failed to get from EhCache: \n" + "   key: " + key, e);
		}
	}

	public void put(K key, V value) {
		Element element = new Element(key, value);
		cache.put(element);
	}

	public void remove(K key) {
		cache.remove(key);
	}

	public void clear() {
		cache.removeAll();
	}

	@Override
	public void flush() {
		cache.flush();
	}

	@Override
	public long getSize() {
		return cache.getSize();
	}

	public static final synchronized void reloadManager() {
		if (manager != null)
			manager.shutdown();
		getManager();
	}

	public static final synchronized CacheManager getManager() {
		if (manager == null) {
			try {
				System.setProperty(CacheManager.ENABLE_SHUTDOWN_HOOK_PROPERTY, "TRUE");
				URL resource = Cache.class.getResource("/cache.xml");
				manager = CacheManager.create(Paths.get(resource.toURI()).toFile().getAbsolutePath());
			} catch (Exception e) {
				log.error("Cannot initialize the cache manager");
				return null;
			}
		}
		return manager;
	}
}