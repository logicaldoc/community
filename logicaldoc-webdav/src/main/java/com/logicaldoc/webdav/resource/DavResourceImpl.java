package com.logicaldoc.webdav.resource;

import static org.apache.commons.io.comparator.ExtensionFileComparator.EXTENSION_COMPARATOR;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
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
import org.apache.jackrabbit.webdav.property.PropEntry;
import org.apache.jackrabbit.webdav.property.ResourceType;
import org.apache.jackrabbit.webdav.xml.Namespace;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ExportContextImpl;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.context.ImportContextImpl;
import com.logicaldoc.webdav.exception.UncheckedDavException;
import com.logicaldoc.webdav.io.manager.IOManager;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.service.ResourceService;
import com.logicaldoc.webdav.session.WebdavSession;
import com.logicaldoc.webdav.web.AbstractWebdavServlet;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.DavResourceImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DavResourceImpl implements DavResource, Serializable {

	private static final String HTTP_LOGICALDOC_COM_NS = "http://logicaldoc.com/ns";

	private static final String RESOURCE_SERVICE = "ResourceService";

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(DavResourceImpl.class);

	protected transient DavResourceFactory factory;

	protected transient WebdavSession session;

	protected transient DavResourceLocator locator;

	protected transient Resource resource;

	protected transient DavPropertySet properties = new DavPropertySet();

	protected boolean propsInitialized = false;

	private boolean isCollection = true;

	protected transient ResourceConfig config;

	private long modificationTime = System.currentTimeMillis();

	protected ResourceService resourceService;

	private transient List<DavResource> list = null;

	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfig config, Resource resource) {

		this.locator = locator;
		this.resource = resource;
		this.factory = factory;
		this.config = config;
		this.session = session;

		resourceService = (ResourceService) Context.get().getBean(RESOURCE_SERVICE);
		if (this.resource != null) {
			this.isCollection = this.resource.isFolder();
			this.resource.setRequestedPerson(Long.parseLong(session.getObject("id").toString()));
		}
	}

	/**
	 * Create a new {@link DavResource}.
	 * 
	 * @param locator resource locator
	 * @param factory resource factory
	 * @param session current session
	 * @param config configuration
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfig config) throws DavException {

		this.factory = factory;
		this.locator = locator;
		this.config = config;
		this.session = session;
		resourceService = (ResourceService) Context.get().getBean(RESOURCE_SERVICE);
	}

	/**
	 * Create a new {@link DavResource}
	 * 
	 * @param locator resource locator
	 * @param factory resource factory
	 * @param session current session
	 * @param config configuration
	 * @param isCollection is the resource folder?
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public DavResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfig config, boolean isCollection) throws DavException {
		this(locator, factory, session, config);
		this.isCollection = isCollection;
		resourceService = (ResourceService) Context.get().getBean(RESOURCE_SERVICE);
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
	 *
	 * @throws IOException if the export fails
	 */
	public void spool(OutputContext outputContext) throws IOException {

		outputContext.setModificationTime(this.resource.getLastModified().getTime());
		outputContext.setETag(this.resource.getETag());
		outputContext.setContentType(AbstractWebdavServlet.getContext().getMimeType(this.resource.getName()));

		if (exists()) {
			ExportContext exportCtx = getExportContext(outputContext);
			if (!config.getIOManager().exportContent(exportCtx, this)) {
				throw new IOException("Unexpected Error while spooling resource.");
			}
		}
	}

	/**
	 * @see DavResource#getProperty(org.apache.jackrabbit.webdav.property.DavPropertyName)
	 */
	public DavProperty<?> getProperty(DavPropertyName name) {
		initProperties();

		Namespace namespace = Namespace.getNamespace(HTTP_LOGICALDOC_COM_NS);
		Namespace ocns = Namespace.getNamespace("oc", HTTP_LOGICALDOC_COM_NS);

		if (!exists())
			return properties.get(name);

		if (name.getNamespace().equals(namespace) && name.getName().equals("id")) {
			addIdProperty(namespace);
		}

		if (name.getName().equals("getetag")) {
			DefaultDavProperty<String> defaultDavProperty = new DefaultDavProperty<>(DavPropertyName.GETETAG,
					resource.getETag());
			properties.add(defaultDavProperty);
		}

		if (name.getNamespace().equals(ocns)) {
			// required
			if (name.getName().equals("id"))
				addIdProperty(ocns);

			// optional
			addPermissionsProperty(name, ocns);

			// optional
			addSizeProperty(name, ocns);
		}

		return properties.get(name);
	}

	private void addPermissionsProperty(DavPropertyName name, Namespace nameSpace) {
		if (name.getName().equals("permissions")) {
			String val = "RDNVCK";
			if (!isCollection() && !resource.isFolder()) {
				val = "RDNVW";
			}
			DefaultDavProperty<String> defaultDavProperty = new DefaultDavProperty<>("permissions", val, nameSpace);
			properties.add(defaultDavProperty);
		}
	}

	private void addSizeProperty(DavPropertyName name, Namespace nameSpace) {
		if (name.getName().equals("size") && !isCollection() && !resource.isFolder()) {
			DefaultDavProperty<Long> defaultDavProperty = new DefaultDavProperty<>("size", resource.getContentLength(),
					nameSpace);
			properties.add(defaultDavProperty);
		}
	}

	private void addIdProperty(Namespace nameSpace) {
		String val = "f-" + resource.getID();
		if (!isCollection() && !resource.isFolder()) {
			val = "d-" + resource.getID();
		}

		DefaultDavProperty<String> idProp = new DefaultDavProperty<>("id", val, nameSpace);
		properties.add(idProp);
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
			properties.add(new DefaultDavProperty<>(DavPropertyName.DISPLAYNAME, getDisplayName()));
		}
		if (isCollection()) {
			properties.add(new ResourceType(ResourceType.COLLECTION));
			// Windows XP support
			properties.add(new DefaultDavProperty<>(DavPropertyName.ISCOLLECTION, "1"));
		} else {
			properties.add(new ResourceType(ResourceType.DEFAULT_RESOURCE));
			// Windows XP support
			properties.add(new DefaultDavProperty<>(DavPropertyName.ISCOLLECTION, "0"));
		}

		/*
		 * set current lock information. If no lock is set to this resource, an
		 * empty lockdiscovery will be returned in the response.
		 */
		properties.add(new DefaultDavProperty<>(DavPropertyName.GETCONTENTLENGTH, this.resource.getContentLength()));

		// Set Dav property LastModified
		long lastmodTime = IOUtil.UNDEFINED_TIME;
		if (this.resource.getLastModified() != null) {
			lastmodTime = this.resource.getLastModified().getTime();
		}

		String lastModified = IOUtil.getLastModified(lastmodTime);
		properties.add(new DefaultDavProperty<>(DavPropertyName.GETLASTMODIFIED, lastModified));

		// Set Dav property CreationDate
		long creationTime = IOUtil.UNDEFINED_TIME;
		if (this.resource.getCreationDate() != null) {
			creationTime = this.resource.getCreationDate().getTime();
		}
		String creationDate = IOUtil.getCreated(creationTime);
		properties.add(new DefaultDavProperty<>(DavPropertyName.CREATIONDATE, creationDate));

		propsInitialized = true;
	}

	/**
	 * @see DavResource#alterProperties(List)
	 *
	 * @param setProperties DAV properties
	 * @param removePropertyNames DAV properties names
	 * 
	 * @return the status
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public MultiStatusResponse alterProperties(DavPropertySet setProperties, DavPropertyNameSet removePropertyNames)
			throws DavException {
		throw new UnsupportedOperationException();
	}

	public MultiStatusResponse alterProperties(List<? extends PropEntry> changeList) throws DavException {

		log.debug("alterProperties");

		MultiStatusResponse msr = new MultiStatusResponse(getHref(), null);

		Namespace ldns = Namespace.getNamespace("oc", HTTP_LOGICALDOC_COM_NS);

		Iterator<? extends PropEntry> it = changeList.iterator();
		while (it.hasNext()) {
			Object o = it.next();

			if (o instanceof DavProperty) {
				log.debug("o instanceof DavProperty");
				DavProperty<?> x = (DavProperty<?>) o;
				if (x.getName().getName().contains("favorite") && x.getName().getNamespace().equals(ldns)) {
					msr.add(x.getName(), HttpServletResponse.SC_OK);
					// add bookmark
					resourceService.addBookmark(this.resource, session);
				} else
					msr.add(x.getName(), HttpServletResponse.SC_FORBIDDEN);
			} else {
				log.debug("o instanceof DavPropertyName");
				DavPropertyName zzz = (DavPropertyName) o;
				if (zzz.getName().equals("favorite") && (zzz.getNamespace().equals(ldns))) {
					msr.add(zzz, HttpServletResponse.SC_NO_CONTENT);
					// remove bookmark
					resourceService.deleteBookmark(this.resource, session);
				} else
					msr.add(zzz, HttpServletResponse.SC_FORBIDDEN);
			}
		}
		return msr;
	}

	/**
	 * @see DavResource#getCollection()
	 * 
	 * @return the resource
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
	 * 
	 * @return an iterator
	 */
	public DavResourceIterator getMembers() {
		if (list != null)
			return new DavResourceIteratorImpl(list);

		list = new ArrayList<>();
		if (exists() && isCollection()) {
			try {
				String path = locator.getResourcePath() == null ? "/" : locator.getResourcePath() + "/";

				List<Resource> resources = resourceService.getChildResources(this.resource);
				Iterator<Resource> resourceIterator = resources.iterator();

				while (resourceIterator.hasNext()) {
					Resource res = resourceIterator.next();

					String currentFilePath = path;
					if (currentFilePath.lastIndexOf("/") == currentFilePath.length() - 1) {
						currentFilePath = currentFilePath + res.getName();
					} else {
						currentFilePath = currentFilePath + "/" + res.getName();
					}

					DavResourceLocator resourceLocator = locator.getFactory().createResourceLocator(locator.getPrefix(),
							"", currentFilePath, false);

					DavResource childRes = factory.createResource(resourceLocator, session);

					list.add(childRes);
				}

			}

			catch (DavException e) {
				throw new UncheckedDavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
			} catch (Exception e) {
				throw new UncheckedDavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
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

		checkConflictAndLocked();

		try {
			String memberName = Text.getName(member.getLocator().getResourcePath());

			ImportContext ctx = getImportContext(inputContext, memberName);

			// Check Write permission on the target folder
			checkWritePermission(member, ctx);

			// Check Add Child permission on the target folder
			checkAddChildPermission(member, ctx);

			/*
			 * LD-Chunked: LD-Chunked LD-Chunk-Size: 1024000 LD-Total-Length:
			 * 5977199 X-LD-Mtime: 1599828016 Authorization: Basic
			 * YWRtaW46YWRtaW4= User-Agent: Mozilla/5.0 (Android)
			 * LogicalDOC-mobile/2.7.0 Host: 192.168.2.7:8083 Content-Length:
			 * 1024000 Content-Type: video/mp4
			 */

			log.debug("LD-Total-Length {}", ctx.getProperty("LD-Total-Length"));

			log.debug("memberName {}", memberName);

			boolean isChunking = false;
			boolean isChunkingComplete = false;

			if (memberName.contains("chunking")) {

				isChunking = true;

				String filext = com.google.common.io.Files.getFileExtension(memberName);
				log.debug("filext {}", filext);

				// get chunk part and total chunks
				String ssss[] = filext.split("-");
				String chunkString = ssss[1];
				int chunkID = Integer.parseInt(ssss[2]);
				int chunkTotal = Integer.parseInt(ssss[3]);
				int chunkPart = Integer.parseInt(ssss[4]);

				log.debug("chunkID {}", chunkID);
				log.debug("chunkPart {}-{}", chunkPart, chunkTotal);

				// Creates directory webdav-chunking in temp dir
				File webdavChunking = new File(FileUtils.getTempDirectory(), "webdav-chunking");
				FileUtils.forceMkdir(webdavChunking);
				Path webdavChunkingPath = webdavChunking.toPath();

				if (chunkPart < chunkTotal) {
					// Save the file on a temp location

					// create a temporary file
					String customFilePrefix = null;
					String customFileSuffix = "." + filext;

					Path tempFile = Files.createTempFile(webdavChunkingPath, customFilePrefix, customFileSuffix);
					log.debug("tempFile {}", tempFile);

					// Write the content on the file
					try (OutputStream stream = new FileOutputStream(tempFile.toFile());) {
						IOUtils.copy(ctx.getInputStream(), stream);
					}
				}

				// Last part of the chunking received
				// Join the parts in a unique filename
				if (chunkPart == (chunkTotal - 1)) {

					isChunkingComplete = true;

					// get all the files in a folder and join them

					log.debug("Join the parts in a unique filename");

					// This filter will only include files ending with .py
					FilenameFilter filter = (dir, fileName) -> fileName.contains(chunkString + "-" + chunkID);

					// We apply the filter
					File[] chunkfiles = webdavChunking.listFiles(filter);

					if (chunkfiles != null) {

						// Check that the number of files is correct
						log.debug("chunkfiles.length {}, chunkTotal {}", chunkfiles.length, chunkTotal);

						if (chunkfiles.length < chunkTotal)
							return;

						// Sort files by extension in ascending order.
						Arrays.sort(chunkfiles, EXTENSION_COMPARATOR);

						mergeChunks(chunkfiles, webdavChunkingPath, memberName, chunkID, ctx);
					}

				}

			}

			if (isChunking && !isChunkingComplete)
				return;

			checkSupportedMediaType(member, ctx);
		} catch (Exception e) {
			handleErrorDuringAddingMember(e);
		}
	}

	private void handleErrorDuringAddingMember(Exception error) throws DavException {
		if (error instanceof DavException)
			throw (DavException) error;

		log.error(error.getMessage(), error);
		throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, error.getMessage());
	}

	private void mergeChunks(File[] chunkfiles, Path webdavChunkingPath, String memberName, int chunkID,
			ImportContext ctx) throws IOException {
		// Checking If The File Exists At The Specified Location
		// Or Not
		Path filePathObj = Files.createTempFile(webdavChunkingPath, null, "merged-" + chunkID);
		try {
			for (File file : chunkfiles) {
				// Appending The New Data To The Existing File
				Files.write(filePathObj, Files.readAllBytes(file.toPath()), StandardOpenOption.APPEND);
			}
			log.debug("! Data Successfully Appended !");

			// Check that the file size of the merged chunks
			// equals to LD-Total-Length

			// Remove all the chunk parts
			for (File toDelete : chunkfiles) {
				FileUtils.deleteQuietly(toDelete);
			}

			String newResourceName = memberName.substring(0, memberName.indexOf("-chunking"));
			log.debug("newResourceName {}", newResourceName);

			// Update the ImportContext: systemId and
			// inputStream
			if (ctx instanceof ImportContextImpl) {
				ImportContextImpl ici = (ImportContextImpl) ctx;
				ici.setSystemId(newResourceName);
				ici.setInputFile(filePathObj.toFile());
			}
		} catch (IOException ioExceptionObj) {
			log.error("Problem Occured While Writing To The File  {}", ioExceptionObj.getMessage());
		}
	}

	private void checkSupportedMediaType(DavResource member, ImportContext ctx) throws IOException, DavException {
		if (!config.getIOManager().importContent(ctx, member)) {
			// any changes should have been reverted in the importer
			throw new DavException(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
		}
	}

	private void checkAddChildPermission(DavResource member, ImportContext ctx) throws DavException {
		if (member.isCollection() && !ctx.getResource().isAddChildEnabled()) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Add Child not allowed on the selected folder");
		}
	}

	private void checkWritePermission(DavResource member, ImportContext ctx) throws DavException {
		if (!member.isCollection() && !ctx.getResource().isWriteEnabled()) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Write Access not allowed on the selected folder");
		}
	}

	private void checkConflictAndLocked() throws DavException {
		if (!exists()) {
			throw new DavException(HttpServletResponse.SC_CONFLICT);
		}
		if (isLocked()) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}
	}

	/**
	 * @see DavResource#removeMember(DavResource)
	 */
	public void removeMember(DavResource member) throws DavException {

		if (!exists() || !member.exists()) {
			throw new DavException(HttpServletResponse.SC_NOT_FOUND);
		}
		if (isLocked()) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}

		Resource res = resourceService.getResource(member.getLocator().getResourcePath(), session);
		// set the requesting person
		res.setRequestedPerson(this.resource.getRequestedPerson());

		if (!res.isDeleteEnabled()) {
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Delete not allowed.");
		}

		resourceService.deleteResource(res, session);
	}

	/**
	 * @see DavResource#move(DavResource)
	 */
	public void move(DavResource destination) throws DavException {

		if (!exists()) {
			throw new DavException(HttpServletResponse.SC_NOT_FOUND);
		}
		if (isLocked()) {
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
				log.debug("name before: {}", name);
				name = name.substring(name.lastIndexOf("/") + 1, name.length()).replace("/default", "");
				log.debug("name after: {}", name);

				Resource parentResource = resourceService.getParentResource(destination.getLocator().getResourcePath(),
						this.resource.getRequestedPerson(), session);

				this.resource.setName(name);
				resourceService.move(this.resource, parentResource, session);
			}
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			throw new UncheckedDavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	/**
	 * @see DavResource#copy(DavResource, boolean)
	 */
	public void copy(DavResource destination, boolean shallow) throws DavException {

		if (!exists()) {
			throw new DavException(HttpServletResponse.SC_NOT_FOUND);
		}
		if (isLocked()) {
			throw new DavException(DavServletResponse.SC_LOCKED);
		}

		if (shallow && isCollection()) {
			// Currently no support for shallow copy; however this is
			// only relevant if the source resource is a collection, because
			// otherwise it doesn't make a difference
			throw new DavException(HttpServletResponse.SC_FORBIDDEN, "Unable to perform shallow copy.");
		}

		try {
			Resource res = resourceService.getResource(destination.getLocator().getResourcePath(), session);
			log.debug("res = {}", res);

			if (res != null) {
				log.debug("res != null");
				res.setName(this.resource.getName());
				Resource destResource = resourceService.getParentResource(res);
				log.debug("destResource.getID() = {}", destResource.getID());

				if (destResource.getRequestedPerson() == 0) {
					destResource.setRequestedPerson((Long.parseLong(session.getObject("id").toString())));
				}

				resourceService.copyResource(destResource, this.resource, session);
			} else {
				log.debug("res IS NULL");
				String name = destination.getLocator().getResourcePath();
				name = name.substring(name.lastIndexOf("/") + 1, name.length()).replace("/default", "");
				if (log.isDebugEnabled())
					log.debug("name = {}", name);

				Resource destResource = resourceService.getParentResource(destination.getResourcePath(),
						this.resource.getRequestedPerson(), session);
				this.resource.setName(name);

				if (destResource.getRequestedPerson() == 0) {
					destResource.setRequestedPerson((Long.parseLong(session.getObject("id").toString())));
				}

				log.debug("destResource.getID() = {}", destResource.getID());
				log.debug("this.resource.getName() = {}", this.resource.getName());
				log.debug("this.resource.getID() = {}", this.resource.getID());

				resourceService.copyResource(destResource, this.resource, session);
			}
		} catch (DavException de) {
			throw de;
		} catch (Exception e) {
			throw new UncheckedDavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
		}
	}

	/**
	 * Checks if a resource can be locked
	 * 
	 * @param type the type
	 * @param scope the scope
	 * 
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
	 * 
	 * @return if there is a lock
	 */
	public boolean hasLock(Type type, Scope scope) {
		return resource.isCheckedOut() == true;
	}

	/**
	 * @see DavResource#getLock(Type, Scope)
	 * 
	 * @return the active lock
	 */
	public ActiveLock getLock(Type type, Scope scope) {
		return new DefaultActiveLock();
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getLocks()
	 * 
	 * @return the active locks
	 */
	public ActiveLock[] getLocks() {
		return new ActiveLock[] {};
	}

	/**
	 * @see DavResource#lock(LockInfo)
	 * 
	 * @return the active lock
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
	 */
	public org.apache.jackrabbit.webdav.DavResourceFactory getFactory() {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see org.apache.jackrabbit.webdav.DavResource#getSession()
	 * 
	 * @return the session
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
	 * Gets the customized factory
	 * 
	 * @return the customized factory
	 */
	protected DavResourceFactory getCostumizedFactory() {
		return this.factory;
	}

	/**
	 * 
	 * @param inputCtx input context
	 * @param systemId identifier of the system
	 * 
	 * @return the import context
	 * 
	 * @see org.apache.jackrabbit.webdav.simple.DavResourceImpl#getImportContext(InputContext,
	 *      String)
	 * 
	 * @throws IOException a generic exception
	 */
	protected ImportContext getImportContext(InputContext inputCtx, String systemId) throws IOException {
		return new ImportContextImpl(resource, systemId, inputCtx);
	}

	/**
	 * @param outputCtx output context
	 * 
	 * @return the export context
	 * 
	 * @see org.apache.jackrabbit.webdav.simple.DavResourceImpl#getExportContext(OutputContext)
	 * 
	 * @throws IOException a generic exception
	 */
	public ExportContext getExportContext(OutputContext outputCtx) throws IOException {
		return new ExportContextImpl(this.resource, outputCtx);
	}

	/**
	 * Return true if this resource cannot be modified due to a write lock that
	 * is not owned by the given session.
	 * 
	 * @return true if this resource cannot be modified due to a write lock
	 */
	private boolean isLocked() {
		return (resource.isCheckedOut() || resource.isLocked());
	}

	@Override
	public void removeProperty(DavPropertyName arg0) throws DavException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setProperty(DavProperty<?> arg0) throws DavException {
		throw new UnsupportedOperationException();
	}
}
