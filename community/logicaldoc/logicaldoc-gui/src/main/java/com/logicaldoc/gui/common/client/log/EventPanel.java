package com.logicaldoc.gui.common.client.log;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.logicaldoc.gui.common.client.beans.GUIEvent;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

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

		ToolStripButton log = AwesomeFactory.newIconButton("clipboard-list", "lastevents");
		log.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				EventsWindow.get().show();
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

	private void prepareLabel(final String text, String style) {
		if (statusLabel != null && contains(statusLabel))
			removeMember(statusLabel);
		statusLabel = new Label(text);
		addMember(statusLabel, 2);

		statusLabel.setStyleName(style);
		statusLabel.setWrap(false);
		statusLabel.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				EventsWindow.get().show();
				statusLabel.setContents("");
			}
		});

		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				statusLabel.setContents("");
				return false;
			}
		}, 15000);
	}

	public void error(String message, String detail) {
		prepareLabel(message, "footerError");
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.ERROR);
		EventsWindow.get().addEvent(event);
		setVisible(true);
	}

	public void warn(String message, String detail) {
		prepareLabel(message, "footerWarn");
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.WARNING);
		EventsWindow.get().addEvent(event);
		setVisible(true);
	}

	public void info(String message, String detail) {
		prepareLabel(message, "footerInfo");
		GUIEvent event = new GUIEvent();
		event.setMessage(message);
		event.setDetail(detail != null ? detail : message);
		event.setSeverity(GUIEvent.INFO);
		EventsWindow.get().addEvent(event);
		setVisible(true);
	}

	public void cleanLabel() {
		statusLabel.setContents("");
	}
}