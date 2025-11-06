package com.logicaldoc.webdav.resource;

import java.io.Serializable;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavMethods;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletResponse;
import org.apache.jackrabbit.webdav.MultiStatus;
import org.apache.jackrabbit.webdav.lock.DefaultActiveLock;
import org.apache.jackrabbit.webdav.lock.Scope;
import org.apache.jackrabbit.webdav.lock.SupportedLock;
import org.apache.jackrabbit.webdav.lock.Type;
import org.apache.jackrabbit.webdav.property.DavPropertyName;
import org.apache.jackrabbit.webdav.property.DefaultDavProperty;
import org.apache.jackrabbit.webdav.property.HrefProperty;
import org.apache.jackrabbit.webdav.version.LabelInfo;
import org.apache.jackrabbit.webdav.version.MergeInfo;
import org.apache.jackrabbit.webdav.version.UpdateInfo;
import org.apache.jackrabbit.webdav.version.VersionControlledResource;
import org.apache.jackrabbit.webdav.version.VersionHistoryResource;
import org.apache.jackrabbit.webdav.version.VersionResource;
import org.apache.jackrabbit.webdav.version.VersionableResource;
import org.apache.jackrabbit.webdav.version.report.ReportType;
import org.apache.jackrabbit.webdav.version.report.SupportedReportSetProperty;
import org.apache.jackrabbit.webdav.xml.Namespace;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.MimeType;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;
import com.logicaldoc.webdav.version.VersionHistoryResourceImpl;

import jakarta.servlet.http.HttpServletResponse;

public class VersionControlledResourceImpl extends DeltaVResourceImpl
		implements VersionControlledResource, Serializable {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(VersionControlledResourceImpl.class);

	public VersionControlledResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, Resource resource) throws DavException {
		super(locator, factory, session, config, resource);
		initSupportedReports();
	}

	/**
	 * Create a new {@link org.apache.jackrabbit.webdav.DavResource}.
	 * 
	 * @param locator resource locator
	 * @param factory factory
	 * @param session the DAV session
	 * @param config configurations
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 */
	public VersionControlledResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config) throws DavException {
		super(locator, factory, session, config);
		initSupportedReports();
	}

	/**
	 * Create a new {@link org.apache.jackrabbit.webdav.DavResource}.
	 * 
	 * @param locator resource locator
	 * @param factory factory
	 * @param session the DAV session
	 * @param config configurations
	 * @param isCollection is this a folder?
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 */
	public VersionControlledResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, boolean isCollection) throws DavException {
		super(locator, factory, session, config, isCollection);
		initSupportedReports();
	}

	// --------------------------------------------------------< DavResource
	// >---
	/**
	 * Return a comma separated string listing the supported method names
	 * 
	 * @return the supported method names
	 * 
	 * @see org.apache.jackrabbit.webdav.DavResource#getSupportedMethods()
	 */
	@Override
	public String getSupportedMethods() {
		StringBuilder sb = new StringBuilder(super.getSupportedMethods());
		// Versioning support
		sb.append(", ").append(VersionableResource.METHODS);
		sb.append(", ").append(DavMethods.METHOD_CHECKOUT);
		sb.append(", ").append(DavMethods.METHOD_UNCHECKOUT);
		sb.append(", ").append(DavMethods.METHOD_LABEL);
		sb.append(", ").append(DavMethods.METHOD_CHECKIN);
		return sb.toString();
	}

	// ------------------------------------------< VersionControlledResource
	// >---
	/**
	 * Adds version control to this resource. If the resource is already under
	 * version control, this method has no effect. If this resource is a
	 * Collection resource this method fails with
	 * {@link DavServletResponse#SC_METHOD_NOT_ALLOWED}.
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException if this resource does
	 *         not exist yet, is a collection or if an error occurs while making
	 *         the underlying node versionable.
	 * @see org.apache.jackrabbit.webdav.version.VersionableResource#addVersionControl()
	 */
	public void addVersionControl() throws DavException {
		// Nothing to do
	}

	/**
	 * Calls {@link javax.jcr.Node#checkin()} on the underlying repository node.
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * @see org.apache.jackrabbit.webdav.version.VersionControlledResource#checkin()
	 */
	public String checkin() throws DavException {
		return null;
	}

	/**
	 * Calls {@link javax.jcr.Node#checkout()} on the underlying repository node
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * 
	 * @see org.apache.jackrabbit.webdav.version.VersionControlledResource#checkout()
	 */
	public void checkout() throws DavException {
		resourceService.checkout(getResource(), getResource().getSession());
	}
	
	@Override
	public org.apache.jackrabbit.webdav.DavSession getSession() {
		return this.getSession();
	}	

	/**
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * 
	 * @see org.apache.jackrabbit.webdav.version.VersionControlledResource#uncheckout()
	 */
	public void uncheckout() throws DavException {
		resourceService.uncheckout(getResource(), getResource().getSession());
	}

	/**
	 * UPDATE feature is not (yet) supported. This method allows fails with
	 * {@link DavServletResponse#SC_NOT_IMPLEMENTED}.
	 * 
	 * @param updateInfo update datails
	 * 
	 * @return the status
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * @see VersionControlledResource#update(UpdateInfo)
	 */
	public MultiStatus update(UpdateInfo updateInfo) throws DavException {
		throw new DavException(HttpServletResponse.SC_NOT_IMPLEMENTED);
	}

	/**
	 * MERGE feature is not (yet) supported. This method allows fails with
	 * {@link DavServletResponse#SC_NOT_IMPLEMENTED}.
	 * 
	 * @param mergeInfo details bout the merge
	 * 
	 * @return the status
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * @see VersionControlledResource#merge(MergeInfo)
	 */
	public MultiStatus merge(MergeInfo mergeInfo) throws DavException {
		throw new DavException(HttpServletResponse.SC_NOT_IMPLEMENTED);
	}

	/**
	 * Modify the labels present with the versions of this resource.
	 * 
	 * @param labelInfo details of the label
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 * @see VersionControlledResource#label(LabelInfo)
	 * @see javax.jcr.version.VersionHistory#addVersionLabel(String, String,
	 *      boolean)
	 * @see javax.jcr.version.VersionHistory#removeVersionLabel(String)
	 */
	public void label(LabelInfo labelInfo) throws DavException {
		if (labelInfo == null) {
			throw new DavException(HttpServletResponse.SC_BAD_REQUEST, "Valid label request body required.");
		}
		if (!exists()) {
			throw new DavException(HttpServletResponse.SC_NOT_FOUND);
		}

		try {
			DavResource[] resArr = this.getReferenceResources(CHECKED_IN);
			if (resArr.length == 1 && resArr[0] instanceof VersionResource versionResource) {
				versionResource.label(labelInfo);
			} else {
				throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
						"DAV:checked-in property on '" + getHref() + "' did not point to a single VersionResource.");
			}
		} catch (Exception e) {
			// Nothing to do
		}
	}

	/**
	 * Returns the {@link javax.jcr.version.VersionHistory} associated with the
	 * repository node. If the node is not versionable an exception is thrown.
	 * 
	 * @return the {@link VersionHistoryResource} associated with this resource.
	 * 
	 * @throws DavException Some error
	 * 
	 * @see org.apache.jackrabbit.webdav.version.VersionControlledResource#getVersionHistory()
	 * @see javax.jcr.Node#getVersionHistory()
	 */
	public VersionHistoryResource getVersionHistory() throws DavException {

		DavResourceLocator loc = getLocatorFromResource(resource);

		try {
			return new VersionHistoryResourceImpl(loc, factory, session, config, resource);
		} catch (DavException e) {
			log.warn(e.getMessage(), e);
		}

		throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "");
	}

	// --------------------------------------------------------------------------
	/**
	 * Define the set of reports supported by this resource.
	 * 
	 * @see SupportedReportSetProperty
	 * @see DeltaVResourceImpl#initSupportedReports()
	 */
	@Override
	protected void initSupportedReports() {
		super.initSupportedReports();
		if (exists()) {
			supportedReports.addReportType(ReportType.LOCATE_BY_HISTORY);
			supportedReports.addReportType(ReportType.VERSION_TREE);
		}

	}

	/**
	 * Fill the property set for this resource.
	 * 
	 * @see DavResourceImpl#initProperties()
	 */
	@Override
	protected void initProperties() {

		if (!propsInitialized) {
			super.initProperties();

			properties.add(new HrefProperty(VERSION_HISTORY, locator.getResourcePath(), true));

			// DAV:auto-version property: there is no auto version, explicit
			// EVENT_CHECKEDOUT is required.
			properties.add(new DefaultDavProperty<>(AUTO_VERSION, null, false));

			if (resource == null)
				return;

			properties.add(new DefaultDavProperty<>(DavPropertyName.DISPLAYNAME, resource.getName(), false));

			properties.add(new DefaultDavProperty<>(DavPropertyName.GETCONTENTTYPE,
					MimeType.getByFilename(resource.getName()), false));

			if (resource.isFolder())
				return;

			SupportedLock supportedLock = new SupportedLock();
			supportedLock.addEntry(Type.WRITE, Scope.EXCLUSIVE);
			properties.add(new DefaultDavProperty<>(DavPropertyName.SUPPORTEDLOCK, supportedLock, false));

			String baseVHref = getLocatorFromResource(resource).getHref(false);

			if (resource.isCheckedOut() || resource.isLocked()) {
				log.debug("{} is checkedout", resource.getName());
				properties.add(new HrefProperty(CHECKED_OUT, baseVHref, true));
				properties.add(new HrefProperty(VersionResource.PREDECESSOR_SET, locator.getResourcePath(), false));

				DefaultActiveLock activeLock = new DefaultActiveLock();
				activeLock.setOwner(resource.getLockUser());
				properties.add(new DefaultDavProperty<>(DavPropertyName.LOCKDISCOVERY, activeLock, false));
				properties.add(new DefaultDavProperty<>("activelock", activeLock, Namespace.XMLNS_NAMESPACE));
			} else {
				properties.add(new HrefProperty(CHECKED_IN, locator.getResourcePath(), true));
			}
		}
	}

	/**
	 * Build a new {@link DavResourceLocator} from the given repository node.
	 * 
	 * @param resource the resource
	 * @return a new locator for the specified node.
	 * @see #getLocatorFromNodePath(String)
	 */
	protected DavResourceLocator getLocatorFromResource(Resource resource) {
		return getLocatorFromNodePath();
	}

	/**
	 * Create a new <code>DavResource</code> from the given locator.
	 * 
	 * @param loc resource locator
	 * 
	 * @return new <code>DavResource</code>
	 * 
	 * @throws org.apache.jackrabbit.webdav.DavException error in the DAV
	 *         communication
	 */
	@Override
	protected DavResource createResourceFromLocator(DavResourceLocator loc) throws DavException {
		return getFactory().createResource(loc, getSession());
	}
}