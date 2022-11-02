package com.logicaldoc.webdav.resource.service;

import java.io.InputStream;
import java.io.Serializable;
import java.util.List;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResourceLocator;

import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * All CRUD as well as versioning functionalities against logicalDOC will be
 * handled through this service pattern. Therefore it is called the most
 * common-class where changes must be done during the time. Security mechanism
 * are also implemented through every method. Therefore the caller
 * (WebDAV-Implementation) has just pass the required path or
 * 
 * @see Resource Object to the appropriated method.
 * 
 * @author Sebastian Wenzky
 */
public interface ResourceService extends Serializable {

	/**
	 * On passing a location as well as the current session you gettin back the
	 * appropriated resource.
	 * 
	 * @param location The location retrieved through (@see DavResourceLocator#getResourcePath())
	 * @param session The current session
	 * 
	 * @return the DAV resource
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public Resource getResource(String location, WebdavSession session) throws DavException;

	/**
	 * On passing a valid
	 * 
	 * @see Resource all resources (Folders as well as documents) will be
	 *      returned - unsorted.
	 * 
	 * @param parentResource The resource(mostly a folder) from which are all
	 *        direct childs has to be fetched and turned back
	 *        s
	 * @return direct childs of the parent resource
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public List<Resource> getChildResources(Resource parentResource) throws DavException;

	/**
	 * Handle of resource creation by passed parent resource as well as the most
	 * import parameters needed for a resource creation.
	 * 
	 * @param parentResource The parent resource(mostly a folder)
	 * @param name the filename and titlename of the child
	 * @param isCollection is the resource folder?
	 * @param context for inputstream
	 * @param session the current session
	 * 
	 * @return the newly created {@link Resource}
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public Resource createResource(Resource parentResource, String name, boolean isCollection, ImportContext context,
			WebdavSession session) throws DavException;

	/**
	 * Updating a resource on passing all new properties though a valid resource
	 * object. This resource object must correspond on the ID attribute with a
	 * valid resource in logicalDOC.
	 * 
	 * @param resource The updateable resource
	 * 
	 * @return the updated resource
	 */
	public Resource updateResource(Resource resource);

	/**
	 * Updating a resource on passing all new properties though a valid resource
	 * object. This resource object must correspond on the ID attribute with a
	 * valid resource in logicalDOC.
	 * 
	 * @param resource The updateable resource
	 * @param context the ImportContext
	 * @param session the current session
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public void updateResource(Resource resource, ImportContext context, WebdavSession session) throws DavException;

	/**
	 * Check for existing resource(logically)
	 * 
	 * @param resource The Resource
	 * 
	 * @return True if found else false
	 */
	public boolean resourceExists(Resource resource);

	/**
	 * Based on the parent resource it will be tried to get a child by a
	 * specific name
	 * 
	 * @param parentResource The parent resource
	 * @param name name of the child must be
	 * 
	 * @return The resource that matches, else it will be returned null
	 */
	public Resource getChildByName(Resource parentResource, String name);

	/**
	 * Based on the current resource location it will be turned back the upper
	 * resource
	 * 
	 * @param resource the current resource
	 * 
	 * @return the parent resource
	 */
	public Resource getParentResource(Resource resource);

	/**
	 * Based on a location path
	 * 
	 * @see DavResourceLocator#getResourcePath() the system tries to get the
	 *      parent resource
	 * 
	 * @param location the resource path
	 * @param userId the user id
	 * @param session the current session
	 * 
	 * @return the parent resource
	 */
	public Resource getParentResource(String location, long userId, WebdavSession session);

	/**
	 * Moves a resource named target to the folder dictionary destination
	 * 
	 * @param target the resource that is involved
	 * @param destination the destination of the move operation
	 * @param session the current session
	 * @return the moved resource
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public Resource move(Resource target, Resource destination, WebdavSession session) throws DavException;

	/**
	 * Sets the stream for a resource
	 * 
	 * @param resource the DAV resource
	 * @param is the stream gor the resource
	 */
	public void streamIn(Resource resource, InputStream is);

	/**
	 * Gets the stream for a resource
	 * 
	 * @param resource the DAV resource
	 * 
	 * @return the stream of the resource
	 */
	public InputStream streamOut(Resource resource);

	/**
	 * Deletion of a resource within logicalDOC.
	 * 
	 * @param resource a valid resource
	 * @param session the current session
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public void deleteResource(Resource resource, WebdavSession session) throws DavException;

	/**
	 * Copying of a resource within logicalDOC. Not supported those days.
	 * 
	 * @param destinResource target resource
	 * @param resource the DAV resource
	 * @param session the current session
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public void copyResource(Resource destinResource, Resource resource, WebdavSession session) throws DavException;

	/**
	 * Versioning-part. Checkout causes the system to make a version
	 * "overwriteable" to put a new version on it.
	 * 
	 * @param resource The appropriated resource that has to be checked out
	 * @param session the current session
	 * 
	 * @throws DavException error in the DAV communication
	 */
	public void checkout(Resource resource, WebdavSession session) throws DavException;

	/**
	 * Versioning-part. Based upon the current resource, the versionhistory is
	 * returned matches to this resource.
	 * 
	 * @param resource The appropriated resource
	 * 
	 * @return All Versions
	 */
	public List<Resource> getHistory(Resource resource);

	/**
	 * Cancels a checkout
	 * 
	 * @param resource the DAV resource
	 * @param session the current session
	 */
	public void uncheckout(Resource resource, WebdavSession session);
}
