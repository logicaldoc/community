package com.logicaldoc.webdav.io.manager;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.context.ImportContext;
import com.logicaldoc.webdav.io.handler.IOHandler;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.IOManagerImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class IOManagerImpl implements IOManager {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(IOManagerImpl.class);

	private final List<IOHandler> ioHandlers = new ArrayList<>();

	public void addIOHandler(IOHandler ioHandler) {
		if (ioHandler == null) {
			throw new IllegalArgumentException("'null' is not a valid IOHandler.");
		}
		ioHandler.setIOManager(this);
		ioHandlers.add(ioHandler);
	}

	public IOHandler[] getIOHandlers() {
		return ioHandlers.toArray(new IOHandler[ioHandlers.size()]);
	}

	public boolean importContent(ImportContext context, boolean isCollection) throws IOException, DavException {
		boolean success = false;
		if (context != null) {

			IOHandler[] handlers = getIOHandlers();
			for (int i = 0; i < handlers.length && !success; i++) {
				IOHandler ioh = handlers[i];
				if (ioh.canImport(context, isCollection)) {
					success = ioh.importContent(context, isCollection);
				}
			}
			context.informCompleted(success);
		}
		return success;
	}

	public boolean importContent(ImportContext context, DavResource resource) throws IOException, DavException {
		boolean success = false;
		if (context != null && resource != null) {

			IOHandler[] handlers = getIOHandlers();
			for (int i = 0; i < handlers.length && !success; i++) {
				IOHandler ioh = handlers[i];
				if (ioh.canImport(context, resource)) {
					success = ioh.importContent(context, resource);
				}
			}
			context.informCompleted(success);
		}
		return success;
	}

	public boolean exportContent(ExportContext context, boolean isCollection) throws IOException {

		boolean success = false;

		if (context != null) {
			IOHandler[] handlers = getIOHandlers();
			for (int i = 0; i < handlers.length && !success; i++) {
				IOHandler ioh = handlers[i];
				if (ioh.canExport(context, isCollection)) {
					success = ioh.exportContent(context, isCollection);
				}
			}
			context.informCompleted(success);
		}
		return success;
	}

	public boolean exportContent(ExportContext context, DavResource resource) throws IOException {

		boolean success = false;

		if (context != null && resource != null) {
			IOHandler[] handlers = getIOHandlers();
			for (int i = 0; i < handlers.length && !success; i++) {
				IOHandler ioh = handlers[i];
				if (ioh.canExport(context, resource)) {
					success = ioh.exportContent(context, resource);
				}
			}
			context.informCompleted(success);
		}
		return success;
	}

	@Override
	public void setIOHandler(List<IOHandler> handler) {
		for (IOHandler myIOHandler : handler) {
			myIOHandler.setIOManager(this);
			this.ioHandlers.add(myIOHandler);
		}
	}

	@Override
	public boolean exportContent(ExportContext context, DavResource resource, Long left, Long rangeLength)
			throws IOException {

		boolean success = false;

		if (context != null && resource != null) {
			IOHandler[] handlers = getIOHandlers();
			for (int i = 0; i < handlers.length && !success; i++) {
				IOHandler ioh = handlers[i];
				if (ioh.canExport(context, resource)) {
					success = ioh.exportContent(context, resource, left, rangeLength);
				}
			}
			context.informCompleted(success);
		}
		return success;
	}
}
