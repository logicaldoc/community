package com.logicaldoc.webdav.web;

import java.net.URI;
import java.net.URISyntaxException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.webdav.AbstractLocatorFactory;
import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavLocatorFactory;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.WebdavRequest;
import org.apache.jackrabbit.webdav.WebdavRequestImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Our own implementation of the {@link WebdavRequest} that corrects the URL
 * encoding bug of Jackrabbit
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8
 */
public class EncodingWebdavRequest extends WebdavRequestImpl {
	
	protected static Logger log = LoggerFactory.getLogger(EncodingWebdavRequest.class);

	private HttpServletRequest request;

	private DavLocatorFactory factory;

	public EncodingWebdavRequest(HttpServletRequest httpRequest, DavLocatorFactory factory) {
		super(httpRequest, factory);
		this.request = httpRequest;
		this.factory = factory;
	}

	@Override
	public DavResourceLocator getDestinationLocator() throws DavException {
		return myGetHrefLocator(request.getHeader(HEADER_DESTINATION), true);
	}

	/**
	 * Replicates the original {@link WebdavRequestImpl#getHrefLocator(String)} but corrects the encoding of spaces by replacing them with the %20
	 * 
	 * @param href the href
	 * @param forDestination the forDestination flag
	 * 
	 * @return the DAV locator
	 * 
	 * @throws DavException a generic DAV error
	 */
	private DavResourceLocator myGetHrefLocator(String href, boolean forDestination) throws DavException {
		String ref = href;
		if (ref != null) {
			// href should be a Simple-ref production as defined in
			// RFC4918, so it is either an absolute URI
			// or an absolute path
			try {
				// This is our customization
				// normalize path (see JCR-3174)
				URI uri = new URI(ref.replace(" ", "%20")).normalize(); 				
				String auth = uri.getAuthority();
				ref = uri.getRawPath();
				if (auth == null) {
					// verify that href is an absolute path
					if (ref.startsWith("//") || !ref.startsWith("/")) {
						log.warn("expected absolute path but found " + ref);
						throw new DavException(HttpServletResponse.SC_BAD_REQUEST);
					}
				} else if (!auth.equals(request.getHeader("Host"))) {
					// this looks like an unsupported cross-server
					// operation, but of course a reverse-proxy
					// might have rewritten the Host header. Since
					// we can't find out, we have to reject anyway.
					// Better use absolute paths in DAV:href
					// elements!
					// TODO enable or disable the comment below while testing
					throw new DavException(HttpServletResponse.SC_FORBIDDEN);
				}
			} catch (URISyntaxException e) {
				log.warn("malformed uri: " + href, e);
				throw new DavException(HttpServletResponse.SC_BAD_REQUEST);
			}
			// cut off the context path
			String contextPath = request.getContextPath();
			if (ref.startsWith(contextPath)) {
				ref = ref.substring(contextPath.length());
			} else {
				// absolute path has to start with context path
				throw new DavException(HttpServletResponse.SC_FORBIDDEN);

			}
		}

		String host = getHeader("Host");
		String scheme = getScheme();
		String uriPrefix = scheme + "://" + host + getContextPath();
		String hrefPrefix = forDestination ? uriPrefix : getContextPath();

		return createResourceLocator(forDestination, ref, hrefPrefix);
	}

	private DavResourceLocator createResourceLocator(boolean forDestination, String ref, String hrefPrefix) {
		if (factory instanceof AbstractLocatorFactory) {
			return ((AbstractLocatorFactory) factory).createResourceLocator(hrefPrefix, ref, forDestination);
		} else {
			return factory.createResourceLocator(hrefPrefix, ref);
		}
	}
}
