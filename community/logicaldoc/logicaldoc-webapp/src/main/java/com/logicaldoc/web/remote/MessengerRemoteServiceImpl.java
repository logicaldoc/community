package com.logicaldoc.web.remote;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.communication.EventListener;
import com.logicaldoc.core.document.AbstractHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.remote.MessageEvent;
import com.logicaldoc.gui.common.client.remote.MessengerRemoteService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.service.DocumentServiceImpl;
import com.logicaldoc.web.service.FolderServiceImpl;

import de.novanic.eventservice.client.event.Event;
import de.novanic.eventservice.service.RemoteEventServiceServlet;

/**
 * A service that forwards the events to the clients
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public class MessengerRemoteServiceImpl extends RemoteEventServiceServlet implements MessengerRemoteService,
		EventListener {

	private static final long serialVersionUID = 1L;

	private EventCollector eventCollector;

	private ContextProperties config;

	private static Logger log = LoggerFactory.getLogger(MessengerRemoteServiceImpl.class);

	private static Set<String> MONITORED_EVENTS = new HashSet<String>(Arrays.asList(new String[] {
			DocumentEvent.STORED.toString(), DocumentEvent.CHANGED.toString(), DocumentEvent.CHECKEDIN.toString(),
			DocumentEvent.CHECKEDOUT.toString(), DocumentEvent.IMMUTABLE.toString(), DocumentEvent.LOCKED.toString(),
			DocumentEvent.UNLOCKED.toString(), DocumentEvent.SIGNED.toString(), DocumentEvent.STAMPED.toString(),
			DocumentEvent.MOVED.toString(), DocumentEvent.DELETED.toString(), DocumentEvent.RENAMED.toString(),
			DocumentEvent.RESTORED.toString(), DocumentEvent.PASSWORD_PROTECTED.toString(),
			DocumentEvent.MOVED.toString(), DocumentEvent.PASSWORD_UNPROTECTED.toString(),
			FolderEvent.RENAMED.toString(), FolderEvent.CREATED.toString(), FolderEvent.CHANGED.toString(),
			FolderEvent.MOVED.toString(), FolderEvent.DELETED.toString() }));

	public void init() {
	}

	@Override
	public void start() {

	}

	private void sendToClient(Event event) {
		addEvent(MessengerRemoteService.MESSAGE_DOMAIN_GUI, event);
	}

	@Override
	public void newEvent(AbstractHistory event) {
		if (EventCollector.isEnabled() && config.getBoolean(event.getTenant() + ".gui.serverpush", false)
				&& MONITORED_EVENTS.contains(event.getEvent())) {

			try {
				MessageEvent message = new MessageEvent(event.getSessionId(), event.getEvent());
				message.setFolderId(event.getFolderId());
				message.setDocId(event.getDocId());

				GUIFolder folder = null;
				if (event.getFolder() != null)
					folder = FolderServiceImpl.fromFolder(event.getFolder());
				else
					folder = FolderServiceImpl.getFolder(null, event.getFolderId());
				message.setFolder(folder);

				GUIDocument document = null;
				if (event.getDocument() != null)
					document = DocumentServiceImpl.fromDocument(event.getDocument(), null, null);
				else if (event.getDocId() != null) {
					DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
					Document d = docDao.findById(event.getDocId());
					if (d != null)
						document = DocumentServiceImpl.fromDocument(event.getDocument(), null, null);
					else {
						document = new GUIDocument();
						document.setId(event.getDocId());
						document.setFileName(event.getFilename());
						document.setFolder(folder);
					}
				}
				message.setDocument(document);

				sendToClient(message);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	public void setEventCollector(EventCollector eventCollector) {
		this.eventCollector = eventCollector;
		this.eventCollector.addListener(this);
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}
}