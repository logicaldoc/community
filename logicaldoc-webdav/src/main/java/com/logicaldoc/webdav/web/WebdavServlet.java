package com.logicaldoc.webdav.web;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.webdav.DavLocatorFactory;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.WebdavRequest;
import org.apache.jackrabbit.webdav.lock.LockManager;
import org.apache.jackrabbit.webdav.lock.SimpleLockManager;
import org.apache.jackrabbit.webdav.simple.LocatorFactoryImplEx;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webdav.resource.DavResourceFactory;
import com.logicaldoc.webdav.resource.DavResourceFactoryImpl;
import com.logicaldoc.webdav.resource.ResourceConfig;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.SimpleWebdavServlet}
 * 
 * @author Sebastian Wenzky
 * 
 */
@SuppressWarnings("serial")
public class WebdavServlet extends AbstractWebdavServlet {

	protected static Logger log = LoggerFactory.getLogger(WebdavServlet.class);

	public static final String INIT_PARAM_RESOURCE_PATH_PREFIX = "resource-path-prefix";

	public static final String INIT_PARAM_AUTHENTICATE_HEADER = "authenticate-header";

	public final static String INIT_PARAM_MISSING_AUTH_MAPPING = "missing-auth-mapping";

	public static final String INIT_PARAM_RESOURCE_CONFIG = "resource-config";

	public static final String CTX_ATTR_RESOURCE_PATH_PREFIX = "jackrabbit.webdav.simple.resourcepath";

	private static String resourcePathPrefix;

	private static LockManager lockManager;

	private static DavResourceFactory resourceFactory;

	private static DavLocatorFactory locatorFactory;

	private static ResourceConfig config;

	public void init() {
		String resourcePathPrefix = getInitParameter(INIT_PARAM_RESOURCE_PATH_PREFIX);
		if (resourcePathPrefix == null) {
			log.debug("Missing path prefix > setting to empty string.");
			resourcePathPrefix = "";
		} else if (resourcePathPrefix.endsWith("/")) {
			log.debug("Path prefix ends with '/' > removing trailing slash.");
			resourcePathPrefix = resourcePathPrefix.substring(0, resourcePathPrefix.length() - 1);
		}
		getServletContext().setAttribute(CTX_ATTR_RESOURCE_PATH_PREFIX, resourcePathPrefix);
		log.info(INIT_PARAM_RESOURCE_PATH_PREFIX + " = '" + resourcePathPrefix + "'");
		setResourcePathPrefix(resourcePathPrefix);

		String configParam = getInitParameter(INIT_PARAM_RESOURCE_CONFIG);
		if (configParam != null) {
			try {
				setConfig(getResourceConfig());
			} catch (Exception e) {
				log.debug("Unable to build resource filter provider.");
			}
		}
	}

	private static void setResourcePathPrefix(String resourcePathPrefix) {
		WebdavServlet.resourcePathPrefix = resourcePathPrefix;
	}

	private static void setLockManager(LockManager lockManager) {
		WebdavServlet.lockManager = lockManager;
	}

	private static void setConfig(ResourceConfig config) {
		WebdavServlet.config = config;
	}

	/**
	 * {@inheritDoc}
	 */
	protected boolean isPreconditionValid(WebdavRequest request, DavResource resource) {
		if (resource.getDisplayName().toLowerCase().equals("thumbs.db"))
			return false;

		return !resource.exists() || request.matchesIfHeader(resource);
	}

	public DavLocatorFactory getLocatorFactory() {
		if (locatorFactory == null) {
			setLocatorFactory(new LocatorFactoryImplEx(resourcePathPrefix));
		}
		return locatorFactory;
	}

	private LockManager getLockManager() {
		if (lockManager == null) {
			setLockManager(new SimpleLockManager());
		}
		return lockManager;
	}

	public DavResourceFactory getResourceFactory() {
		if (resourceFactory == null) {
			setResourceFactory(new DavResourceFactoryImpl(getLockManager(), getResourceConfig()));
		}
		return resourceFactory;
	}

	public ResourceConfig getResourceConfig() {
		// fallback if no config present
		if (config == null)
			setConfig((ResourceConfig) Context.get().getBean(ResourceConfig.class));
		return config;
	}

	public void service(HttpServletRequest request, HttpServletResponse response) {
		ContextProperties settings = Context.get().getProperties();

		// Check if the service is enabled
		if ("true".equals(settings.get("webdav.enabled")))
			super.service(request, response);
		else
			try {
				response.sendError(HttpServletResponse.SC_MOVED_TEMPORARILY);
			} catch (Throwable e) {
				// Nothing to do
			}

	}

	private static void setResourceFactory(DavResourceFactory resourceFactory) {
		WebdavServlet.resourceFactory = resourceFactory;
	}

	private static void setLocatorFactory(DavLocatorFactory locatorFactory) {
		WebdavServlet.locatorFactory = locatorFactory;
	}
}
