package com.logicaldoc.webdav.resource;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletRequest;

import com.logicaldoc.webdav.session.WebdavSession;
import org.apache.commons.lang3.tuple.Pair;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.DavResourceFactory}
 * 
 * @author Sebastian Wenzky
 * 
 */
public interface DavResourceFactory {

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request, WebdavSession session) throws DavException;

	public DavResource createResource(DavResourceLocator locator, DavServletRequest request) throws DavException;

	public DavResource createResource(DavResourceLocator locator, WebdavSession session) throws DavException;
	
	public DavResource createRangeResource(DavResourceLocator locator, WebdavSession session, Pair<String, String> parsedRange) throws DavException;

}