package com.logicaldoc.gui.common.client.websockets;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.realityforge.gwt.websockets.client.WebSocket;
import org.realityforge.gwt.websockets.client.WebSocketListenerAdapter;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.client.rpc.SerializationStreamFactory;
import com.google.gwt.user.client.rpc.SerializationStreamReader;
import com.google.gwt.user.client.rpc.SerializationStreamWriter;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestController;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LimitedQueue;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.dashboard.chat.ChatController;
import com.logicaldoc.gui.frontend.client.services.ReadingRequestService;
import com.smartgwt.client.types.EdgeName;
import com.smartgwt.client.widgets.notify.Notify;
import com.smartgwt.client.widgets.notify.NotifySettings;

/**
 * Listens to events coming from websockets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class WebSocketListener extends WebSocketListenerAdapter {

	private static final String COMMAND = "command";

	private static Set<String> moniteredEvents = new HashSet<>();

	// Maintain a fifos for the history IDs. Key is the class name, value is a
	// FIFO queue
	private Map<String, Queue<Long>> fifos = new HashMap<>();

	static {
		moniteredEvents.addAll(Arrays.asList("event.changed", "event.renamed", "event.checkedin", "event.checkedout",
				"event.locked", "event.unlocked", "event.immutable", "event.signed", "event.stamped", "event.indexed",
				"event.password.protected", "event.password.unprotected", "event.stored", "event.moved",
				"event.deleted", "event.folder.renamed", "event.folder.changed", "event.folder.deleted",
				"event.folder.created", "event.folder.moved", "event.workflowstatus", "event.user.messagereceived",
				"event.chat.newmessage", "event.user.login", "event.user.logout", "event.user.timeout",
				"event.reading.confirmed", "event.reading.requested", COMMAND));
	}

	/**
	 * Puts the history in the relative FIFO
	 * 
	 * @param event
	 * @return true if it was not remembered already, false otherwise
	 */
	private boolean rememberHistory(WebsocketMessage event) {
		Queue<Long> fifo = fifos.get(event.getEvent());
		if (fifo == null) {
			fifo = new LimitedQueue<>(1000);
			fifos.put(event.getEvent(), fifo);
		}

		if (fifo.contains(event.getId()))
			return false;
		else {
			fifo.add(event.getId());
			return true;
		}
	}

	/**
	 * Here there is the trick, the Async Service that is usual return by the
	 * deferred binding is also an instance of a SerializationStreamFactory.
	 * That can be used for serialize and deserialize objects
	 * 
	 * @param message the message to serialize
	 * 
	 * @return the message serialized in a string
	 */
	public String serializeMessage(WebsocketMessage message) {
		try {
			SerializationStreamFactory factory = (SerializationStreamFactory) GWT
					.create(WebsocketsMessageService.class);
			SerializationStreamWriter writer = factory.createStreamWriter();
			writer.writeObject(message);
			return writer.toString();
		} catch (final SerializationException e) {
			GuiLog.error(e.getMessage(), null, e);
		}
		return null;
	}

	public WebsocketMessage deserializeMessage(String data) {
		try {
			SerializationStreamFactory factory = (SerializationStreamFactory) GWT
					.create(WebsocketsMessageService.class);
			final SerializationStreamReader streamReader = factory.createStreamReader(data);
			return (WebsocketMessage) streamReader.readObject();
		} catch (final SerializationException e) {
			GuiLog.error(e.getMessage(), null, e);
		}
		return null;
	}

	private void onEvent(WebsocketMessage event) {
		if (!moniteredEvents.contains(event.getEvent()))
			return;

		if (!rememberHistory(event))
			return;

		handleEvent(event);
	}

	private void handleEvent(WebsocketMessage event) {
		if (isDocumentModifiedEvent(event)) {
			handleDocumentModifiedEvent(event);
		} else if ("event.stored".equals(event.getEvent())) {
			handleStoredEvent(event);
		} else if ("event.moved".equals(event.getEvent())) {
			handleMovedEvent(event);
		} else if ("event.deleted".equals(event.getEvent())) {
			DocumentController.get().deleted(Arrays.asList(event.getDocument()));
		} else if (isFolderEvent(event)) {
			handleFolderEvent(event);
		} else if ("event.user.messagereceived".equals(event.getEvent()) && Menu.enabled(Menu.MESSAGES)) {
			handleMessageReceived(event);
		} else if ("event.user.login".equals(event.getEvent())) {
			UserController.get().loggedIn(event.getUsername());
		} else if (isLogoutEvent(event)) {
			UserController.get().loggedOut(event.getUsername());
		} else if ("event.chat.newmessage".equals(event.getEvent())) {
			ChatController.get().newMessage(event.getId(), event.getDate(), event.getUsername(), event.getComment());
		} else if ("event.reading.confirmed".equals(event.getEvent())) {
			ReadingRequestController.get().confirmReading(event.getDocId());
		} else if ("event.reading.requested".equals(event.getEvent())) {
			String recipient = event.getComment().substring(event.getComment().indexOf(':') + 1).trim();
			if (Session.get().getUser().getUsername().equals(recipient)) {
				ReadingRequestService.Instance.get()
						.getUnconfimedReadings(new AsyncCallback<List<GUIReadingRequest>>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(List<GUIReadingRequest> readings) {
								ReadingRequestController.get().addUnconfirmedReadings(readings);
							}
						});
			}
		} else if (isCommandEvent(event)) {
			processCommand(event);
		}
	}

	private boolean isFolderEvent(WebsocketMessage event) {
		return event.getEvent().startsWith("event.folder.");
	}

	private boolean isCommandEvent(WebsocketMessage event) {
		return COMMAND.equals(event.getEvent()) && Session.get().getSid().equals(event.getSid());
	}

	private boolean isLogoutEvent(WebsocketMessage event) {
		return "event.user.logout".equals(event.getEvent()) || "event.user.timeout".equals(event.getEvent());
	}

	private void handleFolderEvent(WebsocketMessage event) {
		if ("event.folder.renamed".equals(event.getEvent()) || "event.folder.changed".equals(event.getEvent())) {
			FolderController.get().modified(event.getFolder());
		} else if ("event.folder.deleted".equals(event.getEvent())) {
			FolderController.get().deleted(event.getFolder());
		} else if ("event.folder.created".equals(event.getEvent())) {
			FolderController.get().created(event.getFolder());
		} else if ("event.folder.moved".equals(event.getEvent())) {
			FolderController.get().moved(event.getFolder());
		}
	}

	private void handleDocumentModifiedEvent(WebsocketMessage event) {
		if (FolderController.get().getCurrentFolder().getId() == event.getDocument().getFolder().getId()) {
			event.getDocument().setFolder(FolderController.get().getCurrentFolder());
		}
		DocumentController.get().modified(event.getDocument());
	}

	private void handleStoredEvent(WebsocketMessage event) {
		if (FolderController.get().getCurrentFolder().getId() == event.getDocument().getFolder().getId())
			event.getDocument().setFolder(FolderController.get().getCurrentFolder());
		DocumentController.get().stored(event.getDocument());
	}

	private void handleMovedEvent(WebsocketMessage event) {
		if (FolderController.get().getCurrentFolder().getId() == event.getDocument().getFolder().getId())
			event.getDocument().setFolder(FolderController.get().getCurrentFolder());
		DocumentController.get().moved(event.getDocument());
	}

	private void handleMessageReceived(WebsocketMessage event) {
		if (event.getUserId() != null && event.getUserId() == Session.get().getUser().getId()) {
			Session.get().getUser().setUnreadMessages(Session.get().getUser().getUnreadMessages() + 1);
			UserController.get().changed(Session.get().getUser());

			NotifySettings settings = new NotifySettings();
			settings.setDuration(Session.get().getConfigAsInt("gui.popup.timeout") * 1000);
			settings.setMessagePriority(Notify.MESSAGE);
			settings.setPosition(EdgeName.T);

			Notify.addMessage(
					"<b>" + I18N.message("newmessagefrom", event.getAuthor()) + "</b>:<br/><br/>" + event.getComment(),
					null, null, settings);
		}
	}

	private boolean isDocumentModifiedEvent(WebsocketMessage event) {
		return "event.changed".equals(event.getEvent()) || "event.renamed".equals(event.getEvent())
				|| "event.checkedin".equals(event.getEvent()) || "event.checkedout".equals(event.getEvent())
				|| "event.locked".equals(event.getEvent()) || "event.unlocked".equals(event.getEvent())
				|| "event.immutable".equals(event.getEvent()) || "event.signed".equals(event.getEvent())
				|| "event.indexed".equals(event.getEvent()) || "event.stamped".equals(event.getEvent())
				|| "event.password.protected".equals(event.getEvent())
				|| "event.password.unprotected".equals(event.getEvent())
				|| "event.workflowstatus".equals(event.getEvent());
	}

	/**
	 * Method dedicated to the processing of command events that are those
	 * messages with event 'command'
	 * 
	 * @param event The command to process
	 */
	private void processCommand(WebsocketMessage event) {
		if ("openurl".equals(event.getCommand())) {
			WindowUtils.openUrl(event.getPayload(), event.getTarget() != null ? event.getTarget() : "_blank", null);
		} else if ("message".equals(event.getCommand())) {
			if (event.getTarget() == null || "info".equals(event.getTarget()))
				GuiLog.info(event.getPayload());
			else if ("warn".equals(event.getTarget()))
				GuiLog.warn(event.getPayload(), null);
			else if ("error".equals(event.getTarget()))
				GuiLog.error(event.getPayload(), null, null);
		}
	}

	@Override
	public void onMessage(WebSocket webSocket, String data) {
		onEvent(deserializeMessage(data));
	}

	@Override
	public void onError(WebSocket webSocket) {
		if (Session.get().getConfigAsBoolean("gui.serverpush.showerror"))
			GuiLog.warn(I18N.message("serverconnectionerror"), null);
	}
}