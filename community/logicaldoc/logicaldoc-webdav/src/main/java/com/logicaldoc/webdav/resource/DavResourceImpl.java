package com.logicaldoc.webdav.resource;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.jackrabbit.server.io.IOUtil;
import org.apache.jackrabbit.util.Text;
import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceIterator;
import org.apache.jackrabbit.webdav.DavResourceIteratorImpl;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletResponse;
import org.apache.jackrabbit.webdav.MultiStatusResponse;
import org.apache.jackrabbit.webdav.io.InputContext;
import org.apache.jackrabbit.webdav.io.OutputContext;
import org.apache.jackrabbit.webdav.lock.ActiveLock;
import org.apache.jackrabbit.webdav.lock.DefaultActiveLock;
import org.apache.jackrabbit.webdav.lock.LockInfo;
import org.apache.jackrabbit.webdav.lock.LockManager;
import org.apache.jackrabbit.webdav.lock.Scope;
import org.apache.jackrabbit.webdav.lock.Type;
import org.apache.jackrabbit.webdav.property.DavProperty;
import org.apache.jackrabbit.webdav.property.DavPropertyName;
import org.apache.jackrabbit.webdav.property.DavPropertyNameSet;
import org.apache.jackrabbit.webdav.property.DavPropertySet;
import org.apache.jackrabbit.webdav.property.DefaultDavProperty;
import org.apache.jackrabbit.webdav.property.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ExportContextImpl;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.context.ImportContextImpl;
import com.logicaldoc.webdav.io.manager.IOManager;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.service.ResourceService;
import com.logicaldoc.webdav.session.DavSession;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.DavResourceImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceImpl implements DavResource, Serializable {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(DavResourceImpl.class);

	protected DavResourceFactory factory;

	protected DavSession session;

	protected DavResourceLocator locator;

	protected Resource resource;

	protected DavPropertySet properties = new DavPropertySet();

	protected boolean propsInitialized = false;

	private boolean isCollection = true;

	protected ResourceConfig config;

	private long modificationTime = System.currentTimeMillis();

	protected ResourceService resourceService;

	private List<DavResource> list = null;

	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, DavSession session,
			ResourceConfig config, Resource resource) {
		this.locator = locator;
		this.resource = resource;
		this.factory = factory;
		this.config = config;
		this.session = session;

		resourceService = (ResourceService) Context.get().getBean("ResourceService");
		if (this.resource != null) {
			this.isCollection = this.resource.isFolder();
			this.resource.setRequestedPerson(Long.parseLong(session.getObject("id").toString()));
		}
	}

	/**
	 * Create a new {@link DavResource}.
	 * 
	 * @param locator
	 * @param factory
	 * @param session
	 * 
	 */
	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, DavSession session,
			ResourceConfig config) throws DavException {

		this.factory = factory;
		this.locator = locator;
		this.config = config;
		this.session = session;
		resourceService = (ResourceService) Context.get().getBean("ResourceService");
	}

	/**
	 * Create a new {@link DavResource}.
	 * 
	 * @param locator
	 * @param factory
	 * @param session
	 * @param config
	 * @param isCollection
	 * @throws DavException
	 */
	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, DavSession session,
			ResourceConfig config, boolean isCollection) throws DavException {
		this(locator, factory, session, config);
		this.isCollection = isCollection;
		resourceService = (ResourceService) Context.get().getBean("ResourceService");
	}

	/**
	 * @return DavResource#COMPLIANCE_CLASS
	 * @see org.apache.jackrabbit.webdav.DavResource#getComplianceClass()
	 */
	public String getComplianceClass() {
		return "1";
	}

	/**
	 * @return DavResource#METHODS
	 * @see org.apache.jackrabbit.webdav.DavResource#getSupportedMethods()
	 */
	public String getSupportedMethods() {
		return "OPTIONS, GET, HEAD, POST, TRACE, PROPFIND, PROPPATCH, MKCOL, COPY, PUT, DELETE, MOVE";
	}

	/**
	 * @see DavResource#exists() )
	 */
	public boolean exists() {
		return resource != null;
	}

	/**
	 * @see DavResource#isCollection()
	 */
	public boolean isCollection() {
		return isCollection;
	}

	/**
	 * Package protected method that allows to define whether this resource
	 * represents a collection or not.
	 * 
	 * @param isCollection
	 * @deprecated Use the constructor taking a boolean flag instead.
	 */
	void setIsCollection(boolean isCollection) {
		this.isCollection = isCollection;
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getLocator()
	 */
	public DavResourceLocator getLocator() {
		return locator;
	}

	/**
	 * @see DavResource#getResourcePath()
	 */
	public String getResourcePath() {
		return locator.getResourcePath();
	}

	/**
	 * @see DavResource#getHref()
	 */
	public String getHref() {
		return locator.getHref(isCollection());
	}

	/**
	 * Returns the the last segment of the resource path.
	 * <p>
	 * Note that this must not correspond to the name of the underlying
	 * repository item for two reasons:
	 * <ul>
	 * <li>SameNameSiblings have an index appended to their item name.</li>
	 * <li>the resource path may differ from the item path.</li>
	 * </ul>
	 * Using the item name as DAV:displayname caused problems with XP built-in
	 * client in case of resources representing SameNameSibling nodes.
	 * 
	 * @see DavResource#getDisplayName()
	 */
	public String getDisplayName() {
		String resPath = getResourcePath();
		return (resPath != null) ? Text.getName(resPath) : resPath;
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getModificationTime()
	 */
	public long getModificationTime() {

		log.debug("getModificationTime()");

		initProperties();
		return this.modificationTime;
	}

	/**
	 * If this resource exists and the specified context is not
	 * <code>null</code> this implementation build a new {@link ExportContext}
	 * based on the specified context and forwards the export to its
	 * <code>IOManager</code>. If the
	 * {@link IOManager#exportContent(ExportContext, DavResource)} fails, an
	 * <code>IOException</code> is thrown.
	 * 
	 * @see DavResource#spool(OutputContext)
	 * @see ResourceConfig#getIOManager()
	 * @throws IOException if the export fails.
	 */
	public void spool(OutputContext outputContext) throws IOException {
		if (exists() && outputContext != null) {
			ExportContext exportCtx = getExportContext(outputContext);
			if (!config.getIOManager().exportContent(exportCtx, this)) {
				throw new IOException("Unexpected Error while spooling resource.");
			}
		}
	}

	/**
	 * @see DavResource#getProperty(org.apache.jackrabbit.webdav.property.DavPropertyName)
	 */
	public DavProperty getProperty(DavPropertyName name) {
		initProperties();

		log.debug("getProperty(..) " + name);

		return properties.get(name);
	}

	/**
	 * @see DavResource#getProperties()
	 */
	public DavPropertySet getProperties() {
		initProperties();
		return properties;
	}

	/**
	 * @see DavResource#getPropertyNames()
	 */
	public DavPropertyName[] getPropertyNames() {
		return getProperties().getPropertyNames();
	}

	/**
	 * Fill the set of properties
	 */
	protected void initProperties() {
		if (!exists() || propsInitialized) {
			return;
		}

		// set (or reset) fundamental properties
		if (getDisplayName() != null) {
			properties.add(new DefaultDavProperty(DavPropertyName.DISPLAYNAME, getDisplayName()));
		}
		if (isCollection()) {
			properties.add(new ResourceType(ResourceType.COLLECTION));
			// Windows XP support
			properties.add(new DefaultDavProperty(DavPropertyName.ISCOLLECTION, "1"));
		} else {
			properties.add(new ResourceType(ResourceType.DEFAULT_RESOURCE));
			// Windows XP support
			properties.add(new DefaultDavProperty(DavPropertyName.ISCOLLECTION, "0"));
		}

		/*
		 * set current lock information. If no lock is set to this resource, an
		 * empty lockdiscovery will be returned in the response.
		 */
		// properties.add(new LockDiscovery(getLock(Type.WRITE,
		// Scope.EXCLUSIVE)));
		/* lock support information: all locks are lockable. */
		// SupportedLock supportedLock = new SupportedLock();
		// supportedLock.addEntry(Type.WRITE, Scope.EXCLUSIVE);
		// properties.add(supportedLock);
		properties.add(new DefaultDavProperty(DavPropertyName.GETCONTENTLENGTH, this.resource.getContentLength()));

		// Set Dav property LastModified
		long lastmodTime = IOUtil.UNDEFINED_TIME;
		if (this.resource.getLastModified() != null) {
			lastmodTime = this.resource.getLastModified().getTime();
		}

		String lastModified = IOUtil.getLastModified(lastmodTime);
		properties.add(new DefaultDavProperty(DavPropertyName.GETLASTMODIFIED, lastModified));

		// Set Dav property CreationDate

		long creationTime = IOUtil.UNDEFINED_TIME;
		if (this.resource.getCreationDate() != null) {
			creationTime = this.resource.getCreationDate().getTime();
		}
		String creationDate = IOUtil.getCreated(creationTime);
		properties.add(new DefaultDavProperty(DavPropertyName.CREATIONDATE, creationDate));

		propsInitialized = true;
	}

	/**
	 * @see DavResource#alterProperties(DavPropertySet, DavPropertyNameSet)
	 */
	public MultiStatusResponse alterProperties(DavPropertySet setProperties, DavPropertyNameSet removePropertyNames)
			throws DavException {
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("rawtypes")
	public MultiStatusResponse alterProperties(List changeList) throws DavException {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see DavResource#getCollection()
	 */
	public DavResource getCollection() {
		DavResource parent = null;
		if (getResourcePath() != null && !getResourcePath().equals("/")) {
			String parentPath = Text.getRelativeParent(getResourcePath(), 1);
			if (parentPath.equals("")) {
				parentPath = "/";
			}
			DavResourceLocator parentloc = null;
			try {
				parentloc = locator.getFactory().createResourceLocator(locator.getPrefix(), "/", parentPath);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}

			try {
				parent = factory.createResource(parentloc, session);
			} catch (DavException e) {
				// should not occur
				log.error(e.getMessage(), e);
			}
		}
		return parent;
	}

	/**
	 * @see DavResource#getMembers()
	 */
	public DavResourceIterator getMembers() {
		if (list != null)
			return new DavResourceIteratorImpl(list);

		list = new ArrayList<DavResource>();
		if (exists() && isCollection()) {
			try {
				String path = locator.getResourcePath() == null ? "/" : locator.getResourcePath() + "/";

				List<Resource> resources = resourceService.getChildResources(this.resource);
				Iterator<Resource> resourceIterator = resources.iterator();

				while (resourceIterator.hasNext()) {
					Resource resource = resourceIterator.next();

					String currentFilePath = path;
					if (currentFilePath.lastIndexOf("/") == currentFilePath.length() - 1) {
						currentFilePath = currentFilePath + resource.getName();
					} else {
						currentFilePath = currentFilePath + "/" + resource.getName();
					}

					DavResourceLocator resourceLocator = locator.getFactory().createResourceLocator(
							locator.getPrefix(), "", currentFilePath, false);

					DavResource childRes = factory.createResource(resourceLocator, session);

					list.add(childRes);
				}

			}

			catch (DavException e) {
				log.error(e.getMessage());
				throw new RuntimeException(e);
			} catch (Exception e) {
				log.error(e.getMessage());
				throw new RuntimeException(e);
			}
		}
		return new DavResourceIteratorImpl(list);
	}

	/**
	 * Adds a new member to this resource.
	 * 
	 * @see DavResource#addMember(DavResource,
	 *      org.apache.jackrabbit.webdav.io.InputContext)
	 */
	public void addMember(DavResource member, InputContext inputContext) throws DavException {
		if (!exists()) {
			throw new DavException(DavServletResponse.SC_CONFLICT);
		}
		if (isLocked(this) || isLocked(member)) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}

		try {
			String memberName = Text.getName(member.getLocator().getResourcePath());

			ImportContext ctx = getImportContext(inputContext, memberName);

			if (!config.getIOManager().importContent(ctx, member)) {
				// any changes should have been reverted in the importer
				throw new DavException(DavServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
			}
		} catch (Exception e) {
			// log.error(e.getMessage(), e);
			e.printStackTrace();
		}
	}

	/**
	 * @see DavResource#removeMember(DavResource)
	 */
	public void removeMember(DavResource member) throws DavException {
		if (!exists() || !member.exists()) {
			throw new DavException(DavServletResponse.SC_NOT_FOUND);
		}
		if (isLocked(this) || isLocked(member)) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}

		try {
			Resource resource = resourceService.getResource(member.getLocator().getResourcePath(), session);
			// set the requesting person
			resource.setRequestedPerson(this.resource.getRequestedPerson());

			resourceService.deleteResource(resource, session);
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @see DavResource#move(DavResource)
	 */
	public void move(DavResource destination) throws DavException {
		if (!exists()) {
			throw new DavException(DavServletResponse.SC_NOT_FOUND);
		}
		if (isLocked(this)) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}
		try {
			Resource res = resourceService.getResource(destination.getLocator().getResourcePath(), session);
			if (res != null) {
				res.setName(this.resource.getName());
				Resource parentResource = resourceService.getParentResource(res);
				resourceService.move(this.resource, parentResource, session);
			} else {
				String name = destination.getLocator().getResourcePath();
				name = name.substring(name.lastIndexOf("/") + 1, name.length()).replace("/default", "");

				Resource parentResource = resourceService.getParentResource(destination.getLocator().getResourcePath(),
						this.resource.getRequestedPerson(), session);
				this.resource.setName(name);

				resourceService.move(this.resource, parentResource, session);
			}
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @see DavResource#copy(DavResource, boolean)
	 */
	public void copy(DavResource destination, boolean shallow) throws DavException {

		if (!exists()) {
			throw new DavException(DavServletResponse.SC_NOT_FOUND);
		}
		if (isLocked(destination)) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}

		if (shallow && isCollection()) {
			// Currently no support for shallow copy; however this is
			// only relevant if the source resource is a collection, because
			// otherwise it doesn't make a difference
			throw new DavException(DavServletResponse.SC_FORBIDDEN, "Unable to perform shallow copy.");
		}

		try {
			Resource res = resourceService.getResource(destination.getLocator().getResourcePath(), session);
			log.debug("res = " + res);

			if (res != null) {
				log.debug("res != null");
				res.setName(this.resource.getName());
				Resource destResource = resourceService.getParentResource(res);
				log.debug("destResource.getID() = " + destResource.getID());
				log.debug("destResource.getPath() = " + destResource.getPath());

				if (destResource.getRequestedPerson() == 0) {
					destResource.setRequestedPerson((Long.parseLong(session.getObject("id").toString())));
				}

				resourceService.copyResource(destResource, this.resource, session);
			} else {
				log.debug("res IS NULL");
				String name = destination.getLocator().getResourcePath();
				name = name.substring(name.lastIndexOf("/") + 1, name.length()).replace("/default", "");
				log.debug("name = " + name);

				Resource destResource = resourceService.getParentResource(destination.getResourcePath(),
						this.resource.getRequestedPerson(), session);
				this.resource.setName(name);

				if (destResource.getRequestedPerson() == 0) {
					destResource.setRequestedPerson((Long.parseLong(session.getObject("id").toString())));
				}

				log.debug("destResource.getID() = " + destResource.getID());
				log.debug("destResource.getPath() = " + destResource.getPath());

				log.debug("this.resource.getName() = " + this.resource.getName());
				log.debug("this.resource.getPath() = " + this.resource.getPath());
				log.debug("this.resource.getID() = " + this.resource.getID());

				resourceService.copyResource(destResource, this.resource, session);
			}
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		}
	}

	/**
	 * @param type
	 * @param scope
	 * @return true if type is {@link Type#WRITE} and scope is
	 *         {@link Scope#EXCLUSIVE}
	 * @see DavResource#isLockable(org.apache.jackrabbit.webdav.lock.Type,
	 *      org.apache.jackrabbit.webdav.lock.Scope)
	 */
	public boolean isLockable(Type type, Scope scope) {
		return (resource.isCheckedOut() == false);
	}

	/**
	 * @see DavResource#hasLock(org.apache.jackrabbit.webdav.lock.Type,
	 *      org.apache.jackrabbit.webdav.lock.Scope)
	 */
	public boolean hasLock(Type type, Scope scope) {
		return resource.isCheckedOut() == true;
	}

	/**
	 * @see DavResource#getLock(Type, Scope)
	 */
	public ActiveLock getLock(Type type, Scope scope) {
		return new DefaultActiveLock();
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getLocks()
	 */
	public ActiveLock[] getLocks() {
		return new ActiveLock[] {};
	}

	/**
	 * @see DavResource#lock(LockInfo)
	 */
	public ActiveLock lock(LockInfo lockInfo) throws DavException {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see DavResource#refreshLock(LockInfo, String)
	 */
	public ActiveLock refreshLock(LockInfo lockInfo, String lockToken) throws DavException {
		return new DefaultActiveLock();
	}

	/**
	 * @see DavResource#unlock(String)
	 */
	public void unlock(String lockToken) throws DavException {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see DavResource#addLockManager(org.apache.jackrabbit.webdav.lock.LockManager)
	 */
	public void addLockManager(LockManager lockMgr) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getFactory()
	 * @deprecated JackRabbit usage
	 */
	public org.apache.jackrabbit.webdav.DavResourceFactory getFactory() {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getSession()
	 */
	public org.apache.jackrabbit.webdav.DavSession getSession() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Returns the current resource that holds this object
	 * 
	 * @return the resource
	 */
	protected Resource getResource() {
		return this.resource;
	}

	/**
	 * 
	 * @return
	 */
	protected DavResourceFactory getCostumizedFactory() {
		return this.factory;
	}

	/**
	 * @see org.apache.jackrabbit.webdav.simple.DavResourceImpl#getImportContext(InputContext,
	 *      String)
	 */
	protected ImportContext getImportContext(InputContext inputCtx, String systemId) throws IOException {
		return new ImportContextImpl(resource, systemId, inputCtx);
	}

	/**
	 * @see org.apache.jackrabbit.webdav.simple.DavResourceImpl#getExportContext(OutputContext)
	 */
	protected ExportContext getExportContext(OutputContext outputCtx) throws IOException {
		return new ExportContextImpl(this.resource, outputCtx);
	}

	/**
	 * Return true if this resource cannot be modified due to a write lock that
	 * is not owned by the given session.
	 * 
	 * @return true if this resource cannot be modified due to a write lock
	 */
	private boolean isLocked(DavResource res) {
		return (resource.isCheckedOut() || resource.isLocked());
	}

	@Override
	public void removeProperty(DavPropertyName arg0) throws DavException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setProperty(DavProperty arg0) throws DavException {
		throw new UnsupportedOperationException();
	}
}
