package com.logicaldoc.web.websockets;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.server.rpc.impl.ServerSerializationStreamWriter;
import com.logicaldoc.core.History;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.communication.EventListener;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.websockets.WebsocketMessage;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.service.DocumentServiceImpl;
import com.logicaldoc.web.service.FolderServiceImpl;

/**
 * Websockets end-point to distribute events.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
@ServerEndpoint(value = "/wk-event")
public class EventEndpoint implements EventListener {

	private static final int FIFO_SIZE = 1000;

	// Maintain a fifos for the history IDs. Key is the class name, value is a
	// FIFO queue
	private Map<String, Queue<Long>> fifos = new HashMap<>();

	private static Logger log = LoggerFactory.getLogger(EventEndpoint.class);

	private static Set<String> MONITORED_EVENTS = new HashSet<>(Arrays.asList(new String[] {
			DocumentEvent.STORED.toString(), DocumentEvent.CHANGED.toString(), DocumentEvent.CHECKEDIN.toString(),
			DocumentEvent.CHECKEDOUT.toString(), DocumentEvent.IMMUTABLE.toString(), DocumentEvent.LOCKED.toString(),
			DocumentEvent.UNLOCKED.toString(), DocumentEvent.SIGNED.toString(), DocumentEvent.STAMPED.toString(),
			DocumentEvent.MOVED.toString(), DocumentEvent.DELETED.toString(), DocumentEvent.RENAMED.toString(),
			DocumentEvent.INDEXED.toString(), DocumentEvent.RESTORED.toString(),
			DocumentEvent.PASSWORD_PROTECTED.toString(), DocumentEvent.MOVED.toString(),
			DocumentEvent.PASSWORD_UNPROTECTED.toString(), FolderEvent.RENAMED.toString(),
			FolderEvent.CREATED.toString(), FolderEvent.CHANGED.toString(), FolderEvent.MOVED.toString(),
			FolderEvent.DELETED.toString(), UserEvent.MESSAGE_RECEIVED.toString(), UserEvent.LOGIN.toString(),
			UserEvent.LOGOUT.toString(), UserEvent.TIMEOUT.toString(), "event.chat.newmessage" }));

	private static Set<Session> peers = Collections.synchronizedSet(new HashSet<Session>());

	private boolean registered = false;

	/**
	 * Puts the history in the relative FIFO
	 * 
	 * @param history
	 * @return true if it was not remembered already, false otherwise
	 */
	private boolean rememberHistory(History history) {
		Queue<Long> fifo = fifos.get(history.getClass().getName());
		if (fifo == null) {
			fifo = new CircularFifoQueue<>(FIFO_SIZE);
			fifos.put(history.getClass().getName(), fifo);
		}

		if (fifo.contains(history.getId()))
			return false;
		else {
			fifo.add(history.getId());
			return true;
		}
	}

	@OnOpen
	public void onOpen(final Session session) {
		if (!registered) {
			EventCollector eventCollector = (EventCollector) Context.get().getBean(EventCollector.class);
			eventCollector.addListener(this);
			registered = true;
		}

		log.debug("onOpen({})", session.getId());
		peers.add(session);
	}

	@OnClose
	public void onClose(final Session session) {
		log.debug("onClose({})", session.getId());
		peers.remove(session);
	}

	@OnMessage
	public void onMessage(final String message, final Session session) {
		log.debug("onMessage({},{})", message, session.getId());
	}

	@OnMessage
	public void onBinaryMessage(final byte[] data, final Session session) {
		log.debug("onBinaryMessage({})", session.getId());
	}

	@Override
	public void newEvent(History event) {
		ContextProperties config = Context.get().getProperties();

		if (event.getTenant() == null) {
			TenantDAO tenantDAO = (TenantDAO) Context.get().getBean(TenantDAO.class);
			event.setTenant(tenantDAO.getTenantName(event.getTenantId()));
		}

		if (EventCollector.isEnabled() && config.getBoolean(event.getTenant() + ".gui.serverpush", false)
				&& MONITORED_EVENTS.contains(event.getEvent()) && event.isNotifyEvent()) {

			if (!rememberHistory(event))
				return;

			try {
				WebsocketMessage message = prepareMessage(event);
				distributeMessage(message);
			} catch (Exception e) {
				if (log.isDebugEnabled())
					log.debug("Skip sending the websocket message related to the event {}", event, e);
			}
		}
	}

	private WebsocketMessage prepareMessage(History event) throws PersistenceException, ServerException {
		WebsocketMessage message = new WebsocketMessage(event.getSessionId(), event.getEvent());
		message.setFolderId(event.getFolderId());
		message.setDocId(event.getDocId());
		message.setUserId(event.getUserId());
		message.setUsername(event.getUserLogin());
		message.setComment(event.getComment());
		message.setDate(event.getDate());
		message.setId(event.getId());

		if (event instanceof UserHistory)
			message.setAuthor(((UserHistory) event).getAuthor());

		GUIFolder folder = null;
		if (event.getFolder() != null) {
			String color = event.getFolder().getColor();
			folder = new FolderServiceImpl().fromFolder(event.getFolder(), true);
			folder.setColor(color);
		} else if (event.getFolderId() != null)
			folder = new FolderServiceImpl().getFolder(null, event.getFolderId(), true);
		if (folder != null)
			message.setFolder(folder);

		GUIDocument document = null;
		if (event.getDocument() != null) {
			Document clone = new Document(event.getDocument());
			// Report some attributes skipped by the clone method
			clone.setCustomId(event.getDocument().getCustomId());
			clone.setStatus(event.getDocument().getStatus());

			// Put ID 0 in order to convert to GUIDocument without
			// picking up ifos from DB
			clone.setId(0L);
			document = DocumentServiceImpl.fromDocument(clone, null, null);
			document.setId(event.getDocId());
		} else if (event.getDocId() != null) {
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			Document d = docDao.findById(event.getDocId());
			if (d != null) {
				document = DocumentServiceImpl.fromDocument(d, null, null);
			} else {
				document = new GUIDocument();
				document.setId(event.getDocId());
				document.setFileName(event.getFilename());
				document.setFolder(folder);
			}
		}

		if (document != null && (event.getEvent().equals(DocumentEvent.CHECKEDOUT.toString())
				|| event.getEvent().equals(DocumentEvent.LOCKED.toString()))) {
			document.setLockUser(event.getUsername());
			document.setLockUserId(event.getUserId());
		}

		message.setDocument(document);
		return message;
	}

	@OnError
	public void error(Session session, Throwable t) {
		log.warn(t.getMessage());
		log.debug(t.getMessage(), t);
	}

	/**
	 * Distributes a message to all the connected clients
	 * 
	 * @param message The message to be sent
	 */
	public static void distributeMessage(WebsocketMessage message) {
		for (Session peer : peers)
			try {
				peer.getAsyncRemote().sendText(serializeMessage(message));
			} catch (SerializationException e) {
				log.error("Error preparing websocket message {}", message.getEvent(), e);
			}
	}

	private static String serializeMessage(final WebsocketMessage messageDto) throws SerializationException {
		ServerSerializationStreamWriter serverSerializationStreamWriter = new ServerSerializationStreamWriter(
				new SimpleSerializationPolicy());
		serverSerializationStreamWriter.writeObject(messageDto);
		String result = serverSerializationStreamWriter.toString();
		return result;
	}
}