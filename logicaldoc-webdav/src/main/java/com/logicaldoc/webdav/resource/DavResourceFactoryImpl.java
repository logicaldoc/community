package com.logicaldoc.webdav.resource;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavMethods;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.service.ResourceService;
import com.logicaldoc.webdav.session.WebdavSession;

import jakarta.servlet.http.HttpServletResponse;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.ResourceFactoryImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceFactoryImpl implements DavResourceFactory {

	private static final Logger log = LoggerFactory.getLogger(DavResourceFactoryImpl.class);

	private static final Pattern versionRequestPattern = Pattern.compile("/vstore/(\\d.\\d)/(.*)");

	private final ResourceConfiguration resourceConfig;

	private ResourceService resourceService;

	public DavResourceFactoryImpl() {
		this.resourceConfig = Context.get(ResourceConfiguration.class);
		this.resourceService = (ResourceService) Context.get("ResourceService");
	}

	public DavResourceFactoryImpl(ResourceConfiguration resourceConfig) {
		this.resourceConfig = (resourceConfig != null) ? resourceConfig : Context.get(ResourceConfiguration.class);
		this.resourceService = (ResourceService) Context.get("ResourceService");
	}

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request) throws DavException {
		return createResource(locator, request, (WebdavSession) request.getDavSession());
	}

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request, WebdavSession session)
			throws DavException {

		try {
			String resourcePath = locator.getResourcePath();
			Matcher matcher = versionRequestPattern.matcher(locator.getResourcePath());

			String version = null;
			if (matcher.matches()) {
				version = matcher.group(1);
				resourcePath = resourcePath.replaceFirst("/vstore/" + version, "");
			}

			DavResource resource;

			log.debug("createResource resourcePath: {}", resourcePath);
			Resource repositoryResource = resourceService.getResource(resourcePath, session);

			if (repositoryResource == null) {
				boolean isCollection = DavMethods.isCreateCollectionRequest(request);
				resource = createNullResource(locator, session, isCollection);
			} else {
				repositoryResource.setVersionLabel(version);
				repositoryResource.setRequestedPerson(Long.parseLong(session.getObject("id").toString()));
				resource = new VersionControlledResourceImpl(locator, this, session, resourceConfig,
						repositoryResource);
			}

			return resource;
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	public DavResource createResource(DavResourceLocator locator, WebdavSession session) throws DavException {
		try {
			Resource res = resourceService.getResource(locator.getResourcePath(), session);
			return createResource(locator, session, res);
		} catch (Exception e) {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e);
		}
	}

	private DavResource createNullResource(DavResourceLocator locator, WebdavSession session, boolean isCollection)
			throws DavException {
		return new VersionControlledResourceImpl(locator, this, session, resourceConfig, isCollection);
	}

	public DavResource createResource(DavResourceLocator locator, WebdavSession session, Resource resource)
			throws DavException {
		return new VersionControlledResourceImpl(locator, this, session, resourceConfig, resource);
	}

	@Override
	public DavResource createRangeResource(DavResourceLocator locator, WebdavSession session,
			Pair<String, String> parsedRange) throws DavException {
		Resource res = resourceService.getResource(locator.getResourcePath(), session);
		return new RangeResourceImpl(locator, this, session, resourceConfig, res, parsedRange);
	}
}