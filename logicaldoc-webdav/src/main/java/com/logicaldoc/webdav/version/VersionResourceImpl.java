package com.logicaldoc.webdav.version;

import java.util.Collections;
import java.util.List;

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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.resource.DavResourceFactory;
import com.logicaldoc.webdav.resource.DeltaVResourceImpl;
import com.logicaldoc.webdav.resource.ResourceConfiguration;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;

import jakarta.servlet.http.HttpServletResponse;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.VersionResourceImpl}
 * 
 * @author wenzkseb
 * 
 */
public class VersionResourceImpl extends DeltaVResourceImpl implements VersionResource {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(VersionResourceImpl.class);

	public VersionResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, Resource item) throws DavException {
		super(locator, factory, session, config, item);

	}

	@Override
	public DavResourceIterator getMembers() {
		return new DavResourceIteratorImpl(Collections.emptyList());
	}

	@Override
	public void addMember(DavResource member, InputContext inputContext) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@Override
	public void removeMember(DavResource member) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
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
	public void label(LabelInfo labelInfo) throws DavException {
		// Nothing to do
	}

	@Override
	public VersionHistoryResource getVersionHistory() throws DavException {
		return null;
	}

	@Override
	protected void initSupportedReports() {
		super.initSupportedReports();
		if (exists()) {
			supportedReports.addReportType(ReportType.VERSION_TREE);
		}
	}

	@Override
	protected void initProperties() {
		if (!propsInitialized) {
			super.initProperties();

			properties.add(new DefaultDavProperty<>(VERSION_NAME, resource.getVersionLabel(), true));

			String creationDate = IOUtil.getCreated(resource.getVersionDate().getTime());
			properties.add(new DefaultDavProperty<>(DavPropertyName.CREATIONDATE, creationDate));
			properties.add(new HrefProperty(VersionResource.VERSION_HISTORY,
					locator.getResourcePath() + resource.getID(), true));
			properties.add(new DefaultDavProperty<>(DeltaVConstants.COMMENT, resource.getComment()));
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