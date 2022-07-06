package com.logicaldoc.gui.common.client.log;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEvent;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.notify.Notify;
import com.smartgwt.client.widgets.notify.NotifySettings;

/**
 * Small panel showing the last event message. If the user clicks it, a list of
 * all recent events is displayed.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EventPanel extends HLayout {

	private static EventPanel instance;

	private Label statusLabel;

	private EventPanel() {
		setHeight(20);
		setWidth100();
		setAlign(Alignment.LEFT);
		setMargin(2);
		setMembersMargin(2);

		Button log = AwesomeFactory.newIconButton("clipboard-list", "lastevents");
		log.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				MessagesWindow.get().show();
				if (statusLabel != null)
					statusLabel.setContents("");
			}
		});

		addMember(log);
	}

	public static EventPanel get() {
		if (instance == null)
			instance = new EventPanel();
		return instance;
	}

	private void prepareLabel(final String text, String priorityLevel) {
		if (statusLabel != null && contains(statusLabel))
			removeMember(statusLabel);

		String oneRow = text.replace('\n', ' ').replaceAll("<", "&lt;").replaceAll(">", "&gt;");
		if (text.length() > 80)
			oneRow = oneRow.substring(0, 80) + "...";

		statusLabel = new Label(oneRow);
		addMember(statusLabel, 2);

		String style = "footerInfo";
		if (priorityLevel.equals(GUIEvent.ERROR))
			style = "footerError";
		else if (priorityLevel.equals(GUIEvent.WARNING))
			style = "footerWarn";

		statusLabel.setStyleName(style);
		statusLabel.setWrap(false);
		statusLabel.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				MessagesWindow.get().show();
				statusLabel.setContents("");
			}
		});

		int popupTimeout = Session.get().getConfigAsInt("gui.popup.timeout") * 1000;

		NotifySettings settings = new NotifySettings();
		settings.setDuration(popupTimeout);
		settings.setMessagePriority(Notify.MESSAGE);
		if (priorityLevel.equals(GUIEvent.ERROR))
			settings.setMessagePriority(Notify.ERROR);
		else if (priorityLevel.equals(GUIEvent.WARNING))
			settings.setMessagePriority(Notify.WARN);

		Notify.addMessage(text, null, null, settings);
		
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				statusLabel.setContents("");
				return false;
			}
		}, popupTimeout);
	}

	public void error(String message, String detail) {
		prepareLabel(message, GUIEvent.ERROR);
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.ERROR);
		MessagesWindow.get().addEvent(event);
	}

	public void warn(String message, String detail) {
		prepareLabel(message, GUIEvent.WARNING);
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.WARNING);
		MessagesWindow.get().addEvent(event);
	}

	public void info(String message, String detail) {
		prepareLabel(message, GUIEvent.INFO);
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.INFO);
		MessagesWindow.get().addEvent(event);
	}

	public void cleanLabel() {
		statusLabel.setContents("");
	}
}