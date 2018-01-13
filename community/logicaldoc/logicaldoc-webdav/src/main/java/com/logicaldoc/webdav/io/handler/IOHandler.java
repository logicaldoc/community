package com.logicaldoc.webdav.io.handler;

import java.io.IOException;
import java.io.Serializable;

import org.apache.jackrabbit.webdav.DavResource;

import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.io.manager.IOManager;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.IOHandler}
 * 
 * @author Sebastian Wenzky
 * 
 */
public interface IOHandler extends Serializable {

	public IOManager getIOManager();

	public void setIOManager(IOManager ioManager);

	public String getName();

	public boolean canImport(ImportContext context, boolean isCollection);

	public boolean canImport(ImportContext context, DavResource resource);

	public boolean importContent(ImportContext context, boolean isCollection) throws IOException;

	public boolean importContent(ImportContext context, DavResource resource) throws IOException;

	public boolean canExport(ExportContext context, boolean isCollection);

	public boolean canExport(ExportContext context, DavResource resource);

	public boolean exportContent(ExportContext context, boolean isCollection) throws IOException;

	public boolean exportContent(ExportContext context, DavResource resource) throws IOException;
}