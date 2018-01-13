package com.logicaldoc.bm.cache;

import java.io.Serializable;
import java.util.Collection;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheException;
import net.sf.ehcache.Element;

/**
 * A thin adapter for <b>Ehcache</b> support.
 * <p>
 * Thread-safety is taken care of by the underlying <b>Ehcache</b>
 * instance.
 *
 * @see org.springframework.cache.ehcache.EhCacheFactoryBean
 * @see org.springframework.cache.ehcache.EhCacheManagerFactoryBean
 */
public class EhCacheAdapter<K extends Serializable, V extends Object>
        implements SimpleCache<K, V>
{
    private net.sf.ehcache.Cache cache;
    
    public EhCacheAdapter()
    {
    }

    /**
     * @param cache the backing Ehcache instance
     */
    public void setCache(Cache cache)
    {
        this.cache = cache;
    }

    public boolean contains(K key)
    {
        try
        {
            return (cache.get(key) != null);
        }
        catch (CacheException e)
        {
            throw new RuntimeException("contains failed", e);
        }
    }

    @SuppressWarnings("unchecked")
    public Collection<K> getKeys()
    {
        return cache.getKeys();
    }

    @SuppressWarnings("unchecked")
    public V get(K key)
    {
        try
        {
            Element element = cache.get(key);
            if (element != null)
            {
                return (V) element.getObjectValue();
            }
            else
            {
                return null;
            }
        }
        catch (IllegalStateException ie)
        {
           throw new RuntimeException("Failed to get from EhCache as state invalid: \n" +
                 "  state: " + cache.getStatus() + "\n" +
                 "   key: " + key,
                 ie);
        }
        catch (CacheException e)
        {
            throw new RuntimeException("Failed to get from EhCache: \n" +
                    "   key: " + key,
                    e);
        }
    }

    public void put(K key, V value)
    {
        Element element = new Element(key, value);
        cache.put(element);
    }

    public void remove(K key)
    {
        cache.remove(key);
    }

    public void clear()
    {
        cache.removeAll();
    }
}
