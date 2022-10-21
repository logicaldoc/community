package com.logicaldoc.webdav.version;

import java.util.Collections;
import java.util.List;

import javax.jcr.RepositoryException;
import javax.jcr.version.VersionHistory;
import javax.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.server.io.IOUtil;
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
import org.apache.jackrabbit.webdav.property.DefaultDavProperty;
import org.apache.jackrabbit.webdav.property.HrefProperty;
import org.apache.jackrabbit.webdav.version.DeltaVConstants;
import org.apache.jackrabbit.webdav.version.LabelInfo;
import org.apache.jackrabbit.webdav.version.VersionControlledResource;
import org.apache.jackrabbit.webdav.version.VersionHistoryResource;
import org.apache.jackrabbit.webdav.version.VersionResource;
import org.apache.jackrabbit.webdav.version.VersionableResource;
import org.apache.jackrabbit.webdav.version.report.ReportType;

import com.logicaldoc.webdav.resource.DavResourceFactory;
import com.logicaldoc.webdav.resource.DeltaVResourceImpl;
import com.logicaldoc.webdav.resource.ResourceConfig;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.VersionResourceImpl}
 * 
 * @author wenzkseb
 * 
 */
public class VersionResourceImpl extends DeltaVResourceImpl implements VersionResource {

	private static final long serialVersionUID = 1L;

	public VersionResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfig config, Resource item) throws DavException {
		super(locator, factory, session, config, item);

	}

	public DavResourceIterator getMembers() {
		return new DavResourceIteratorImpl(Collections.emptyList());
	}

	public void addMember(DavResource member, InputContext inputContext) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	public void removeMember(DavResource member) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	public void setProperty(DavProperty<?> property) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	public void removeProperty(DavPropertyName propertyName) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	public MultiStatusResponse alterProperties(DavPropertySet setProperties, DavPropertyNameSet removePropertyNames)
			throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@SuppressWarnings("rawtypes")
	public MultiStatusResponse alterProperties(List changeList) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	public void label(LabelInfo labelInfo) throws DavException {
		// Nothing to do
	}

	public VersionHistoryResource getVersionHistory() throws DavException {
		return null;
	}

	@SuppressWarnings("unused")
	private VersionHistory getVersionHistoryItem() throws RepositoryException {
		return null;
	}

	protected void initSupportedReports() {
		super.initSupportedReports();
		if (exists()) {
			supportedReports.addReportType(ReportType.VERSION_TREE);
		}
	}

	protected void initProperties() {

		if (!propsInitialized) {
			super.initProperties();

			properties.add(new DefaultDavProperty<String>(VERSION_NAME, resource.getVersionLabel(), true));

			// properties.add(new
			// DefaultDavProperty<String>(DavPropertyName.CREATIONDATE,
			// resource.getVersionDate()));
			String creationDate = IOUtil.getCreated(resource.getVersionDate().getTime());
			properties.add(new DefaultDavProperty<String>(DavPropertyName.CREATIONDATE, creationDate));
			properties.add(new HrefProperty(VersionResource.VERSION_HISTORY,
					locator.getResourcePath() + resource.getID(), true));
			properties.add(new DefaultDavProperty<String>(DeltaVConstants.COMMENT, resource.getComment()));
		}
	}

	@Override
	public String getSupportedMethods() {
		StringBuilder sb = new StringBuilder(super.getSupportedMethods());
		// Versioning support
		sb.append(", ").append(VersionableResource.METHODS);

		try {
			if ((resource.isCheckedOut())) {
				sb.append(", ").append(VersionControlledResource.methods_checkedOut);
			} else {
				sb.append(", ").append(VersionControlledResource.methods_checkedIn);
			}
		} catch (Exception e) {
			// should not occur.
			log.error(e.getMessage());
		}

		return sb.toString();
	}
}
