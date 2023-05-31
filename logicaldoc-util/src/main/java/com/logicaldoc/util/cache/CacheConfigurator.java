package com.logicaldoc.util.cache;

import java.util.Iterator;
import java.util.List;

import org.jdom2.Element;

import com.logicaldoc.util.config.XMLBean;

/**
 * Configurator for the cache.xml file
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class CacheConfigurator {

	private XMLBean xml;

	public CacheConfigurator(String resource) {
		if (getClass().getClassLoader().getResource(resource) != null)
			xml = new XMLBean(getClass().getClassLoader().getResource(resource));
		else
			xml = new XMLBean(resource);
	}

	public CacheConfigurator() {
		xml = new XMLBean(getClass().getClassLoader().getResource("cache.xml"));
	}

	private Element getCacheElement(String cacheName) {
		// Search for the specified cache
		List<Element> caches = xml.getRootElement().getChildren("cache", xml.getRootElement().getNamespace());
		for (Iterator<Element> iterator = caches.iterator(); iterator.hasNext();) {
			Element elem = iterator.next();
			String name = elem.getAttributeValue("name");
			if (cacheName.equals(name))
				return elem;
		}
		return null;
	}

	public boolean containsCache(String cacheName) {
		return getCacheElement(cacheName) != null;
	}

	public void removeCache(String cacheName) {
		Element cacheElement = getCacheElement(cacheName);
		if (cacheElement != null)
			cacheElement.getParent().removeContent(cacheElement);
	}

	public void addCache(String cacheName, int maxElementsInMemory) {
		if (containsCache(cacheName))
			return;

		List<Element> children = xml.getRootElement().getChildren("defaultCache", xml.getRootElement().getNamespace());
		int index = xml.getRootElement().getChildren().indexOf(children.get(0));

		// Prepare the new cache
		Element cache = new Element("cache", xml.getRootElement().getNamespace());
		cache.setAttribute("name", cacheName);
		cache.setAttribute("maxElementsInMemory", "" + maxElementsInMemory);
		cache.setAttribute("eternal", "true");
		cache.setAttribute("overflowToDisk", "true");
		cache.setAttribute("diskPersistent", "true");

		children = xml.getRootElement().getChildren();
		children.add(index + 1, cache);
	}

	public void setCacheDir(String path) {
		List<Element> list = xml.getAllChildren("diskStore");
		Element elem = list.iterator().next();
		elem.setAttribute("path", path);
	}

	public void write() {
		xml.writeXMLDoc();
	}
}