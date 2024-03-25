package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.LoggersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Main log panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class LogPanel extends VLayout {

	private String appender;

	public LogPanel(String appender) {
		this.appender = appender;
		setHeight100();
	}

	@Override
	public void onDraw() {
		final HTMLPane htmlPane = new HTMLPane();
		htmlPane.setWidth100();
		htmlPane.setHeight100();
		htmlPane.setShowEdges(true);
		htmlPane.setContentsURL(GWT.getHostPageBaseURL() + "log?appender=" + appender);
		htmlPane.setContentsType(ContentsType.PAGE);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		SelectItem logFileSelector = ItemFactory.newLogAppenderSelector();
		logFileSelector.setValue(appender);
		logFileSelector.addChangedHandler(changed -> htmlPane
				.setContentsURL(GWT.getHostPageBaseURL() + "log?appender=" + changed.getValue().toString()));

		SelectItem levelSelector = ItemFactory.newLogLevelSelector();

		ComboBoxItem loggerSelector = ItemFactory.newLoggerSelector();

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSaveLogger(levelSelector, loggerSelector));

		ToolStripButton delete = new ToolStripButton(I18N.message("ddelete"));
		delete.setDisabled(true);
		delete.addClickHandler(event -> onDeleteLogger(loggerSelector));

		loggerSelector.addChangedHandler(event -> {
			levelSelector.setValue(loggerSelector.getSelectedRecord().getAttribute("level"));
			delete.setDisabled(loggerSelector.getSelectedRecord().getAttributeAsBoolean("reserved"));
		});

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(event -> {
			htmlPane.redraw();
			htmlPane.setWidth100();
			htmlPane.setHeight100();
		});

		ToolStripButton download = new ToolStripButton(I18N.message("downloadlogs"));
		download.addClickHandler(event -> Util.download(Util.contextPath() + "log?appender=all"));

		if ("DMS_WEB".equals(appender))
			toolStrip.addFormItem(logFileSelector);
		toolStrip.addButton(refresh);
		if (!Session.get().isDemo())
			toolStrip.addButton(download);
		if ("DMS_WEB".equals(appender) && Session.get().isDefaultTenant() && !Session.get().isDemo()) {
			toolStrip.addSeparator();
			toolStrip.addFormItem(loggerSelector);
			toolStrip.addFormItem(levelSelector);
			toolStrip.addButton(save);
			toolStrip.addButton(delete);
		}

		toolStrip.addButton(refresh);
		toolStrip.addFill();
		addMember(toolStrip);
		addMember(htmlPane);
	}

	private void onDeleteLogger(ComboBoxItem loggerSelector) {
		SC.ask(I18N.message("confirmdeletelogger"), choice -> {
			if (Boolean.TRUE.equals(choice) && loggerSelector.getValueAsString() != null
					&& !loggerSelector.getValueAsString().isEmpty()) {
				SystemService.Instance.get().removeLogger(loggerSelector.getValueAsString(), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						loggerSelector.setValue("root");
						loggerSelector.selectValue();
					}
				});
			}
		});
	}

	private void onSaveLogger(SelectItem levelSelector, ComboBoxItem loggerSelector) {
		String name = loggerSelector.getValueAsString();
		if (name != null && !name.isEmpty()) {
			boolean additivity = loggerSelector.getSelectedRecord() != null
					&& name.equals(loggerSelector.getSelectedRecord().getAttributeAsString("name"))
					&& loggerSelector.getSelectedRecord().getAttributeAsBoolean("additivity");
			SystemService.Instance.get().saveLogger(name, levelSelector.getValueAsString(), additivity,
					new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg0) {
							loggerSelector.setOptionDataSource(new LoggersDS());
							loggerSelector.setValue(name);
							loggerSelector.selectValue();
						}
					});
		}
	}
}