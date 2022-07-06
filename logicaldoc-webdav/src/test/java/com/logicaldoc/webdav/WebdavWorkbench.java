package com.logicaldoc.webdav;

import java.io.IOException;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.PersistenceConfiguration;
import net.sf.ehcache.config.PersistenceConfiguration.Strategy;
import net.sf.ehcache.store.MemoryStoreEvictionPolicy;

public class WebdavWorkbench {
	public static void main(String[] args) throws IOException {
		CacheManager cacheManager = CacheManager.newInstance();
		Cache cache = new Cache(
				new CacheConfiguration("webdav", 5000).memoryStoreEvictionPolicy(MemoryStoreEvictionPolicy.LFU)
						.eternal(false).timeToLiveSeconds(60).timeToIdleSeconds(30).diskExpiryThreadIntervalSeconds(0)
						.persistence(new PersistenceConfiguration().strategy(Strategy.LOCALTEMPSWAP)));
		cacheManager.addCache(cache);

		Element entry = new Element(10L, "value1"); 
		cache.put(entry);

		System.out.println("cache: " + cache);
		System.out.println("element: " + cache.get(10L));
	}
}