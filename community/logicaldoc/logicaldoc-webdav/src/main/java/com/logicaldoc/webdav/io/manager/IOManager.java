package com.logicaldoc.webdav.io.manager;

import java.io.IOException;
import java.io.Serializable;
import java.util.List;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;

import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.io.handler.IOHandler;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.IOManager}
 * 
 * @author Sebastian Wenzky
 * 
 */
public interface IOManager extends Serializable {

	public void setIOHandler(List<IOHandler> handler);

	public void addIOHandler(IOHandler ioHandler);

	public IOHandler[] getIOHandlers();

	public boolean importContent(ImportContext context, boolean isCollection) throws IOException, DavException;

	public boolean importContent(ImportContext context, DavResource resource) throws IOException, DavException;

	public boolean exportContent(ExportContext context, boolean isCollection) throws IOException;

	public boolean exportContent(ExportContext context, DavResource resource) throws IOException;

	public boolean exportContent(ExportContext context, DavResource resource, Long left, Long rangeLength) throws IOException;
}
