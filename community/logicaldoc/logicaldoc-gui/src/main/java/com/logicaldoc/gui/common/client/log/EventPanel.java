package com.logicaldoc.gui.common.client.log;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.logicaldoc.gui.common.client.beans.GUIEvent;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Small panel showing the last event message. If the user clicks it, a list of
 * all recent events is displayed.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class EventPanel extends HLayout {
	private static EventPanel instance;

	private Label statusLabel;

	private Img log;

	private Img close;

	private EventPanel() {
		setHeight(20);
		setWidth100();
		setAlign(Alignment.LEFT);
		setMargin(2);
		setMembersMargin(2);

		log = ItemFactory.newImgIcon("logging.png");
		log.setTooltip(I18N.message("lastevents"));
		log.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				EventsWindow.get().show();
				if (statusLabel != null)
					statusLabel.setContents("");
			}
		});

		close = ItemFactory.newImgIcon("delete.png");
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				setVisible(false);
			}
		});
		close.setVisible(false);

		addMember(close);
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

	public boolean isShowLog() {
		return log != null && log.isVisible();
	}

	public void setShowLog(boolean showLog) {
		this.log.setVisible(showLog);
	}

	public boolean isShowClose() {
		return close != null && close.isVisible();
	}

	public void setShowClose(boolean showClose) {
		this.close.setVisible(showClose);
	}

	public void cleanLabel() {
		statusLabel.setContents("");
	}
}