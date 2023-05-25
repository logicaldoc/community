package com.logicaldoc.webdav.version;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceIterator;
import org.apache.jackrabbit.webdav.DavResourceIteratorImpl;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.MultiStatusResponse;
import org.apache.jackrabbit.webdav.io.InputContext;
import org.apache.jackrabbit.webdav.property.DavProperty;
import org.apache.jackrabbit.webdav.property.DavPropertyName;
import org.apache.jackrabbit.webdav.property.DavPropertyNameSet;
import org.apache.jackrabbit.webdav.property.DavPropertySet;
import org.apache.jackrabbit.webdav.property.HrefProperty;
import org.apache.jackrabbit.webdav.property.ResourceType;
import org.apache.jackrabbit.webdav.version.VersionHistoryResource;
import org.apache.jackrabbit.webdav.version.VersionResource;

import com.logicaldoc.webdav.resource.DavResourceFactory;
import com.logicaldoc.webdav.resource.DeltaVResourceImpl;
import com.logicaldoc.webdav.resource.ResourceConfig;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.VersionHistoryResourceImpl}
 * 
 * @author Sebastian Wenzky
 */
public class VersionHistoryResourceImpl extends DeltaVResourceImpl implements VersionHistoryResource {

	private static final long serialVersionUID = 1L;

	public VersionHistoryResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfig config, Resource resource) throws DavException {
		super(locator, factory, session, config, resource);
	}

	@Override
	public DavResourceIterator getMembers() {
		return new DavResourceIteratorImpl(new ArrayList<>());
	}

	@Override
	public void addMember(DavResource member, InputContext inputContext) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@Override
	public void removeMember(DavResource member) throws DavException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setProperty(DavProperty<?> property) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@Override
	public void removeProperty(DavPropertyName propertyName) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@Override
	public MultiStatusResponse alterProperties(DavPropertySet setProperties, DavPropertyNameSet removePropertyNames)
			throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public MultiStatusResponse alterProperties(List changeList) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@Override
	public VersionResource[] getVersions() throws DavException {

		List<Resource> resourceVersions = resourceService.getHistory(resource);

		List<VersionResource> versions = new ArrayList<>();

		for (Resource resource : resourceVersions) {
			DavResourceLocator loc = locator.getFactory().createResourceLocator(locator.getPrefix(),
					locator.getResourcePath() + "/vstore/" + resource.getVersionLabel() + "/" + resource.getName());
			versions.add(new VersionResourceImpl(loc, factory, session, config, resource));
		}

		return versions.toArray(new VersionResource[versions.size()]);

	}

	@Override
	protected void initProperties() {
		if (!propsInitialized) {
			super.initProperties();

			properties.add(new ResourceType(new int[] { ResourceType.COLLECTION, ResourceType.VERSION_HISTORY }));

			try {
				properties.add(new HrefProperty(VersionHistoryResource.ROOT_VERSION, "", false));
			} catch (Exception e) {
				log.error(e.getMessage());
			}
		}
	}
}
