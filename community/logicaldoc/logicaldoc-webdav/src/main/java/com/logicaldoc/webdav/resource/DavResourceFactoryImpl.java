package com.logicaldoc.webdav.resource;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavMethods;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletRequest;
import org.apache.jackrabbit.webdav.DavServletResponse;
import org.apache.jackrabbit.webdav.lock.LockManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.service.ResourceService;
import com.logicaldoc.webdav.session.DavSession;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.ResourceFactoryImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceFactoryImpl implements DavResourceFactory {

	protected static Logger log = LoggerFactory.getLogger(DavResourceFactoryImpl.class);

	private static final Pattern versionRequestPattern = Pattern.compile("/vstore/([0-9].[0-9])/(.*)");

	private final ResourceConfig resourceConfig;

	private ResourceService resourceService;

	public DavResourceFactoryImpl(LockManager lockMgr) {
		this.resourceConfig = (ResourceConfig) Context.get().getBean("ResourceConfig");
		this.resourceService = (ResourceService) Context.get().getBean("ResourceService");
	}

	public DavResourceFactoryImpl(LockManager lockMgr, ResourceConfig resourceConfig) {
		this.resourceConfig = (resourceConfig != null) ? resourceConfig : (ResourceConfig) Context.get()
				.getBean("ResourceConfig");
		this.resourceService = (ResourceService) Context.get().getBean("ResourceService");
	}

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request) throws DavException {
		return createResource(locator, request, (DavSession) request.getDavSession());
	}

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request, DavSession session)
			throws DavException {

		try {
			String resourcePath = locator.getResourcePath();
			Matcher matcher = versionRequestPattern.matcher(locator.getResourcePath());

			String version = null;
			if (matcher.matches() == true) {
				version = matcher.group(1);
				resourcePath = resourcePath.replaceFirst("/vstore/" + version, "");
			}

			DavResource resource;

			Resource repositoryResource = resourceService.getResource(resourcePath, session);

			if (repositoryResource == null) {
				boolean isCollection = DavMethods.isCreateCollectionRequest(request);
				resource = createNullResource(locator, session, isCollection);
			} else {
				repositoryResource.setVersionLabel(version);

				repositoryResource.setRequestedPerson(Long.parseLong(session.getObject("id").toString()));
				resource = new VersionControlledResourceImpl(locator, this, session, resourceConfig, repositoryResource);
			}

			putInCache(session, resource);
			return resource;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw new DavException(DavServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	public DavResource createResource(DavResourceLocator locator, DavSession session) throws DavException {
		try {
			DavResource resource = getFromCache(session, locator.getResourcePath());
			if (resource != null)
				return resource;

			Resource res = resourceService.getResource(locator.getResourcePath(), session);
			resource = createResource(locator, session, res);

			putInCache(session, resource);
			return resource;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new DavException(DavServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	/**
	 * Puts an entry in the cache
	 * 
	 * @param key The entry ID as <userid>;<path>
	 * @param resource The entry to be cached
	 */
	public void putInCache(DavSession session, DavResource resource) {
		try {
			ContextProperties config = Context.get().getProperties();
			if ("true".equals(config.getProperty("webdav.usecache"))) {
				// Initialize the collection of children
				Cache cache = ((CacheManager) Context.get().getBean("DavCacheManager"))
						.getCache("dav-resources");
				Element element = new Element(session.getObject("id") + ";" + resource.getResourcePath(), resource);
				cache.put(element);
			}
		} catch (Throwable t) {

		}
	}

	/**
	 * Retrieves an entry from cache
	 * 
	 * @param key The entry ID as <userid>;<path>
	 * @return The cached entry
	 */
	private DavResource getFromCache(DavSession session, String path) {
		ContextProperties config = Context.get().getProperties();
		if ("true".equals(config.getProperty("webdav.usecache"))) {
			String key = session.getObject("id") + ";" + path;
			Cache cache = ((CacheManager) Context.get().getBean("DavCacheManager")).getCache("dav-resources");
			Element element = cache.get(key);
			DavResource resource = null;
			if (element != null) {
				resource = (DavResource) element.getValue();
				return resource;
			} else
				return null;
		} else
			return null;
	}

	private DavResource createNullResource(DavResourceLocator locator, DavSession session, boolean isCollection)
			throws DavException {

		return new VersionControlledResourceImpl(locator, this, session, resourceConfig, isCollection);
	}

	public DavResource createResource(DavResourceLocator locator, DavSession session, Resource resource)
			throws DavException {
		DavResource res = getFromCache(session, locator.getResourcePath());
		if (res != null)
			return res;

		res = new VersionControlledResourceImpl(locator, this, session, resourceConfig, resource);
		putInCache(session, res);
		return res;
	}
}