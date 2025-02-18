package com.logicaldoc.webdav.web;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.webdav.DavLocatorFactory;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.WebdavRequest;
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

	private static final Logger log = LoggerFactory.getLogger(WebdavServlet.class);

	public static final String INIT_PARAM_RESOURCE_PATH_PREFIX = "resource-path-prefix";

	public static final String INIT_PARAM_AUTHENTICATE_HEADER = "authenticate-header";

	public static final String INIT_PARAM_MISSING_AUTH_MAPPING = "missing-auth-mapping";

	public static final String INIT_PARAM_RESOURCE_CONFIG = "resource-config";

	public static final String CTX_ATTR_RESOURCE_PATH_PREFIX = "jackrabbit.webdav.simple.resourcepath";

	private static String resourcePathPrefix;

	private static DavResourceFactory resourceFactory;

	private static DavLocatorFactory locatorFactory;

	private static ResourceConfig config;
	
	public static boolean foldersizeEnabled = true;

	@Override
	public void init() {
		String resPathPrefix = getInitParameter(INIT_PARAM_RESOURCE_PATH_PREFIX);
		if (resPathPrefix == null) {
			log.debug("Missing path prefix > setting to empty string.");
			resPathPrefix = "";
		} else if (resPathPrefix.endsWith("/")) {
			log.debug("Path prefix ends with '/' > removing trailing slash.");
			resPathPrefix = resPathPrefix.substring(0, resPathPrefix.length() - 1);
		}
		getServletContext().setAttribute(CTX_ATTR_RESOURCE_PATH_PREFIX, resPathPrefix);
		log.info("{} = '{}'", INIT_PARAM_RESOURCE_PATH_PREFIX, resPathPrefix);
		setResourcePathPrefix(resPathPrefix);

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

	private static void setConfig(ResourceConfig config) {
		WebdavServlet.config = config;
	}

	/**
	 * {@inheritDoc}
	 */
	protected boolean isPreconditionValid(WebdavRequest request, DavResource resource) {
		if ("thumbs.db".equalsIgnoreCase(resource.getDisplayName()))
			return false;

		return !resource.exists() || request.matchesIfHeader(resource);
	}

	public DavLocatorFactory getLocatorFactory() {
		if (locatorFactory == null) {
			setLocatorFactory(new LocatorFactoryImplEx(resourcePathPrefix));
		}
		return locatorFactory;
	}

	public DavResourceFactory getResourceFactory() {
		if (resourceFactory == null) {
			setResourceFactory(new DavResourceFactoryImpl(getResourceConfig()));
		}
		return resourceFactory;
	}

	public ResourceConfig getResourceConfig() {
		// fallback if no config present
		if (config == null)
			setConfig(Context.get(ResourceConfig.class));
		return config;
	}

	@Override
	public void service(HttpServletRequest request, HttpServletResponse response) {
		ContextProperties settings = Context.get().getProperties();

		// Check if the service is enabled
		if ("true".equals(settings.get("webdav.enabled")))
			super.service(request, response);
		else
			try {
				response.sendError(HttpServletResponse.SC_MOVED_TEMPORARILY);
			} catch (Exception e) {
				// Nothing to do
			}

		if ("true".equals(settings.get("webdav.foldersize.enabled")))
			WebdavServlet.foldersizeEnabled = true;
		else 
			WebdavServlet.foldersizeEnabled = false;		
	}

	private static void setResourceFactory(DavResourceFactory resourceFactory) {
		WebdavServlet.resourceFactory = resourceFactory;
	}

	private static void setLocatorFactory(DavLocatorFactory locatorFactory) {
		WebdavServlet.locatorFactory = locatorFactory;
	}
}
