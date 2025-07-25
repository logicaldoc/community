package com.logicaldoc.web.websockets;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import jakarta.websocket.OnClose;
import jakarta.websocket.OnError;
import jakarta.websocket.OnMessage;
import jakarta.websocket.OnOpen;
import jakarta.websocket.Session;
import jakarta.websocket.server.ServerEndpoint;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.server.rpc.impl.ServerSerializationStreamWriter;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.communication.EventListener;
import com.logicaldoc.core.document.AbstractDocumentHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.websockets.WebsocketMessage;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;
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

	private static final Logger log = LoggerFactory.getLogger(EventEndpoint.class);

	private static final Set<String> MONITORED_EVENTS = new HashSet<>(Arrays.asList(DocumentEvent.STORED.toString(),
			DocumentEvent.CHANGED.toString(), DocumentEvent.CHECKEDIN.toString(), DocumentEvent.CHECKEDOUT.toString(),
			DocumentEvent.IMMUTABLE.toString(), DocumentEvent.LOCKED.toString(), DocumentEvent.UNLOCKED.toString(),
			DocumentEvent.SIGNED.toString(), DocumentEvent.STAMPED.toString(), DocumentEvent.MOVED.toString(),
			DocumentEvent.DELETED.toString(), DocumentEvent.RENAMED.toString(), DocumentEvent.INDEXED.toString(),
			DocumentEvent.RESTORED.toString(), DocumentEvent.PASSWORD_PROTECTED.toString(),
			DocumentEvent.MOVED.toString(), DocumentEvent.PASSWORD_UNPROTECTED.toString(),
			DocumentEvent.READING_CONFIRMED.toString(), DocumentEvent.READING_REQUESTED.toString(),
			FolderEvent.RENAMED.toString(), FolderEvent.CREATED.toString(), FolderEvent.CHANGED.toString(),
			FolderEvent.MOVED.toString(), FolderEvent.DELETED.toString(), UserEvent.MESSAGE_RECEIVED.toString(),
			UserEvent.LOGIN.toString(), UserEvent.LOGOUT.toString(), UserEvent.TIMEOUT.toString(),
			"event.chat.newmessage"));

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

	/**
	 * Returns the number of canched events of the given type
	 * 
	 * @param historyClass a class of History
	 * 
	 * @return number of cached events
	 */
	public <T extends History> int countQueueSize(Class<T> historyClass) {
		Queue<Long> fifo = fifos.get(historyClass.getName());
		return fifo != null ? fifo.size() : 0;
	}

	@OnOpen
	public void onOpen(final Session session) {
		if (!registered) {
			EventCollector eventCollector = Context.get(EventCollector.class);
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

		try {
			if (event.getTenant() == null) {
				TenantDAO tenantDAO = Context.get(TenantDAO.class);
				event.setTenant(tenantDAO.getTenantName(event.getTenantId()));
			}
		} catch (PersistenceException e) {
			log.warn("Cannot retrieve the name of tenant {}", event.getTenantId());
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
		message.setUserId(event.getUserId());
		message.setUsername(event.getUserLogin());
		message.setComment(event.getComment());
		message.setDate(event.getDate());
		message.setId(event.getId());
		message.setTenantId(event.getTenantId());

		if (event instanceof UserHistory userHistory)
			message.setAuthor(userHistory.getAuthor());

		if (event instanceof AbstractDocumentHistory adh) {
			message.setFolderId(adh.getFolderId());
			message.setDocId(adh.getDocId());

			GUIFolder folder = setFolder(message, adh);

			GUIDocument document = null;
			if (adh.getDocument() != null && adh.getDocument() instanceof Document doc) {
				Document clone = new Document(doc);
				// Report some attributes skipped by the clone method
				clone.setCustomId(adh.getDocument().getCustomId());
				clone.setStatus(adh.getDocument().getStatus());

				// Put ID 0 in order to convert to GUIDocument without
				// picking up ifos from DB
				clone.setId(0L);
				document = new DocumentServiceImpl().fromDocument(clone, null, null);
				document.setId(adh.getDocId());
			} else if (adh.getDocId() != null) {
				DocumentDAO docDao = Context.get(DocumentDAO.class);
				Document d = docDao.findById(adh.getDocId());
				if (d != null) {
					document = new DocumentServiceImpl().fromDocument(d, null, null);
				} else {
					document = new GUIDocument();
					document.setId(adh.getDocId());
					document.setFileName(adh.getFilename());
					document.setFolder(folder);
				}
			}

			if (document != null && (event.getEvent().equals(DocumentEvent.CHECKEDOUT.toString())
					|| event.getEvent().equals(DocumentEvent.LOCKED.toString()))) {
				document.setLockUser(event.getUsername());
				document.setLockUserId(event.getUserId());
			}

			message.setDocument(document);
		}
		return message;
	}

	protected GUIFolder setFolder(WebsocketMessage message, AbstractDocumentHistory adh)
			throws PersistenceException, ServerException {
		GUIFolder folder = null;
		if (adh.getFolder() != null) {
			String color = adh.getFolder().getColor();
			folder = new FolderServiceImpl().fromFolder(adh.getFolder(), true);
			folder.setColor(color);
		} else if (adh.getFolderId() != null)
			folder = new FolderServiceImpl().getFolder(null, adh.getFolderId(), true);
		if (folder != null)
			message.setFolder(folder);
		return folder;
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
		try {
			String serializedMessage = serializeMessage(message);
			for (Session peer : peers)
				if (peer.getAsyncRemote() != null)
					sendMessageToPear(message.getEvent(), serializedMessage, peer);
		} catch (SerializationException e) {
			log.error("Error preparing websocket message {}", message.getEvent());
			log.error(e.getMessage(), e);
		}
	}

	private static synchronized void sendMessageToPear(String event, String serializedMessage, Session peer) {
		try {
			peer.getBasicRemote().sendText(serializedMessage);
		} catch (Exception e) {
			if (e.getMessage().contains("WebSocket session has been closed")) {
				log.debug("Cannot send websocket message {} to peer {} because WebSocket session has been closed",
						event, peer.getRequestURI());
			} else {
				log.error("Error sending websocket message {} to peer {}", event, peer.getRequestURI());
				log.error(e.getMessage(), e);
			}
		}
	}

	private static String serializeMessage(final WebsocketMessage messageDto) throws SerializationException {
		ServerSerializationStreamWriter serverSerializationStreamWriter = new ServerSerializationStreamWriter(
				new SimpleSerializationPolicy());
		serverSerializationStreamWriter.writeObject(messageDto);
		return serverSerializationStreamWriter.toString();
	}
}