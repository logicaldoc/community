package com.logicaldoc.webdav.io.handler;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.server.io.IOUtil;
import org.apache.jackrabbit.server.io.PropertyExportContext;
import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.exception.WebDavStorageException;
import com.logicaldoc.webdav.io.manager.IOManager;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.service.ResourceService;
import com.logicaldoc.webdav.web.AbstractWebdavServlet;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.DefaultHandler}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DefaultHandler implements IOHandler {

	private static final String CANNOT_EXPORT = ": Cannot export ";

	private static final String CANNOT_IMPORT = ": Cannot import ";

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(AbstractWebdavServlet.class);

	private String collectionNodetype = JcrConstants.NT_FOLDER;

	private String defaultNodetype = JcrConstants.NT_FILE;

	/*
	 * IMPORTANT NOTES: for webDAV compliancy the default nodetype of the
	 * content node has been changed from nt:resource to nt:unstructured.
	 */
	private String contentNodetype = JcrConstants.NT_UNSTRUCTURED;

	private ResourceService resourceService;

	public void setResourceService(ResourceService resourceService) {
		this.resourceService = resourceService;
	}

	private IOManager ioManager;

	public DefaultHandler() {
	}

	public DefaultHandler(IOManager ioManager) {
		this.ioManager = ioManager;
	}

	public DefaultHandler(IOManager ioManager, String collectionNodetype, String defaultNodetype,
			String contentNodetype) {
		this.ioManager = ioManager;

		this.collectionNodetype = collectionNodetype;
		this.defaultNodetype = defaultNodetype;
		this.contentNodetype = contentNodetype;
	}

	public IOManager getIOManager() {
		return ioManager;
	}

	public void setIOManager(IOManager ioManager) {
		this.ioManager = ioManager;
	}

	public String getName() {
		return getClass().getName();
	}

	public boolean canImport(ImportContext context, boolean isCollection) {
		if (context == null || context.isCompleted()) {
			return false;
		}
		Resource resource = context.getResource();
		return resource != null;
	}

	public boolean canImport(ImportContext context, DavResource resource) {
		if (resource == null) {
			return false;
		}
		return canImport(context, resource.isCollection());
	}

	public boolean importContent(ImportContext context, boolean isCollection) throws IOException, DavException {
		if (!canImport(context, isCollection)) {
			final String message = getName() + CANNOT_IMPORT + context.getSystemId();
			log.warn(message);
			throw new IOException(message);
		}

		boolean success = false;
		try {
			success = setContentData(context, isCollection);
			if (log.isDebugEnabled())
				log.debug("success = {}", success);
		} catch (DavException e) {
			throw e;
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		return success;
	}

	public boolean importContent(ImportContext context, DavResource resource) throws IOException, DavException {
		if (!canImport(context, resource)) {
			throw new IOException(getName() + CANNOT_IMPORT + context.getSystemId());
		}
		return importContent(context, resource.isCollection());
	}

	public boolean canExport(ExportContext context, boolean isCollection) {
		if (context == null || context.isCompleted()) {
			return false;
		}
		return true;
	}

	public boolean canExport(ExportContext context, DavResource resource) {
		if (resource == null) {
			return false;
		}
		return canExport(context, resource.isCollection());
	}

	public boolean exportContent(ExportContext context, boolean isCollection) throws IOException {
		if (!canExport(context, isCollection)) {
			throw new IOException(getName() + CANNOT_EXPORT);
		}

		if (context.hasStream())
			exportData(context, context.getResource());

		return true;
	}

	public boolean exportContent(ExportContext context, DavResource resource) throws IOException {
		if (!canExport(context, resource)) {
			throw new IOException(getName() + CANNOT_EXPORT);
		}
		return exportContent(context, resource.isCollection());
	}

	protected void exportData(ExportContext context, Resource resource) throws IOException {
		try {
			InputStream is = resourceService.streamOut(resource);
			if (is != null)
				IOUtil.spool(is, context.getOutputStream());
		} catch (FileNotFoundException e) {
			throw new IOException("Can't find file " + resource.getName() + "(" + resource.getID() + ")");
		}
	}

	protected synchronized boolean setContentData(ImportContext context, boolean isCollection) throws DavException {

		Resource resource = context.getResource();
		String name = context.getSystemId();

		Resource res = resourceService.getChildByName(resource, name);

		if (res == null) {
			resourceService.createResource(resource, name, isCollection, context, resource.getSession());
			return true;
		} else {
			res.setRequestedPerson(resource.getRequestedPerson());
			resourceService.updateResource(res, context, res.getSession());
			return true;
		}
	}

	protected Resource getContentNode(ExportContext context) {
		return context.getResource();
	}

	protected String getCollectionNodeType() {
		return collectionNodetype;
	}

	protected String getNodeType() {
		return defaultNodetype;
	}

	protected String getContentNodeType() {
		return contentNodetype;
	}

	public boolean canExport(PropertyExportContext context, boolean isCollection) {
		return canExport((ExportContext) context, isCollection);
	}

	public boolean exportProperties(PropertyExportContext exportContext, boolean isCollection)
			throws WebDavStorageException {
		if (!canExport(exportContext, isCollection)) {
			throw new WebDavStorageException("PropertyHandler " + getName() + " failed to export properties.");
		}

		return true;
	}

	@Override
	public boolean exportContent(ExportContext context, DavResource resource, Long left, Long rangeLength)
			throws IOException {

		if (!canExport(context, resource)) {
			throw new IOException(getName() + CANNOT_EXPORT);
		}

		return exportContent(context, resource.isCollection(), left, rangeLength);
	}

	private boolean exportContent(ExportContext context, boolean isCollection, Long left, Long rangeLength)
			throws IOException {

		if (!canExport(context, isCollection)) {
			throw new IOException(getName() + CANNOT_EXPORT);
		}

		if (context.hasStream())
			exportData(context, context.getResource(), left, rangeLength);

		return true;
	}

	protected void exportData(ExportContext context, Resource resource, Long left, Long rangeLength)
			throws IOException {
		try {
			InputStream is = resourceService.streamOut(resource);
			if (is != null) {
				IOUtils.copyLarge(is, context.getOutputStream(), left, rangeLength);
			}
		} catch (FileNotFoundException e) {
			throw new IOException("Can't find file " + resource.getName() + "(" + resource.getID() + ")");
		}
	}

}