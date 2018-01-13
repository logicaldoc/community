package com.logicaldoc.gui.frontend.client.subscription;

import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This is the form used for subscribe a user to the events.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 7.5
 */
public class SubscriptionDialog extends Window {

	public SubscriptionDialog(final ListGrid grid) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		final ListGridRecord selection = grid.getSelectedRecord();

		setTitle(I18N.message("subscription") + " - " + selection.getAttributeAsString("name"));

		final String[] events;

		String buf = selection.getAttributeAsString("events");
		if (buf == null || buf.isEmpty())
			events = null;
		else if (!buf.contains(","))
			events = new String[] { buf.trim() };
		else {
			events = buf.split(",");
		}

		final ListGridRecord[] selectedRecords = grid.getSelectedRecords();
		final long[] selectedIds = new long[selectedRecords.length];
		for (int i = 0; i < selectedRecords.length; i++) {
			selectedIds[i] = Long.parseLong(selectedRecords[i].getAttributeAsString("id"));
		}

		boolean isFolderSubscription = "folder".equals(selection.getAttributeAsString("type"));

		setWidth(350);
		if (isFolderSubscription)
			setHeight(360);
		else
			setHeight(300);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		SelectItem option = new SelectItem("option", I18N.message("subscriptionoption"));
		option.setWidth(290);
		LinkedHashMap<String, String> options = new LinkedHashMap<String, String>();
		options.put("current", I18N.message("subscribecurrent"));
		options.put("subfolders", I18N.message("subscribesubfolders"));
		option.setValueMap(options);
		option.setValue("1".equals(selection.getAttributeAsString("folderOption")) ? "subfolders" : "current");

		SelectItem notifyon = new SelectItem("notifyon", I18N.message("notifyon"));
		notifyon.setWidth(310);
		LinkedHashMap<String, String> vals = new LinkedHashMap<String, String>();
		vals.put("all", I18N.message("allevents"));
		vals.put("selection", I18N.message("selectedevents"));
		notifyon.setValueMap(vals);
		notifyon.setValue((events == null || events.length == 0) ? "all" : "selection");

		final SelectItem event;
		event = ItemFactory.newEventsSelector("event", I18N.message("event"), true, false, false);
		event.setEndRow(true);
		event.setDisabled(events == null || events.length == 0);
		if (events != null)
			event.setValues(events);

		notifyon.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent e) {
				if ("selection".equals(form.getValueAsString("notifyon")))
					event.setDisabled(false);
				else
					event.setDisabled(true);
			}
		});

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				String[] events = null;
				final String eventsStr;
				final String option = form.getValueAsString("option");
				if ("selection".equals(form.getValueAsString("notifyon"))) {
					String buf = form.getValues().get("event").toString().trim().toLowerCase();
					buf = buf.replace('[', ' ');
					buf = buf.replace(']', ' ');
					eventsStr = buf.replace(" ", "");
					events = eventsStr.split(",");
				} else
					eventsStr = null;

				AuditService.Instance.get().update(selectedIds, option.equals("current"), events,
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								Log.info(I18N.message("settingssaved"), null);
								for (ListGridRecord record : selectedRecords) {
									record.setAttribute("events", eventsStr);
									if (option != null && !option.isEmpty())
										record.setAttribute("folderOption", option.equals("current") ? "0" : "1");
									grid.refreshRow(grid.getRecordIndex(record));
								}
							}
						});
				destroy();
			}
		});

		if (isFolderSubscription)
			form.setItems(option, notifyon, event, save);
		else
			form.setItems(notifyon, event, save);

		addItem(form);
	}

	/**
	 * Constructor used to subscribe a user to folders or documents
	 */
	public SubscriptionDialog(final Long folderId, final long[] docIds) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		if (folderId != null)
			setTitle(I18N.message("foldersubscription"));
		else
			setTitle(I18N.message("documentsubscription"));

		setWidth(290);
		if (folderId != null)
			setHeight(360);
		else
			setHeight(300);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(6);
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		SelectItem option = new SelectItem("option", I18N.message("subscriptionoption"));
		option.setWidth(280);
		LinkedHashMap<String, String> options = new LinkedHashMap<String, String>();
		options.put("current", I18N.message("subscribecurrent"));
		options.put("subfolders", I18N.message("subscribesubfolders"));
		option.setValueMap(options);
		option.setValue("current");

		SelectItem notifyon = new SelectItem("notifyon", I18N.message("notifyon"));
		notifyon.setWidth(280);
		LinkedHashMap<String, String> vals = new LinkedHashMap<String, String>();
		vals.put("all", I18N.message("allevents"));
		vals.put("selection", I18N.message("selectedevents"));
		notifyon.setValueMap(vals);
		notifyon.setValue("all");

		final SelectItem event;
		if (folderId != null)
			event = ItemFactory.newEventsSelector("event", I18N.message("event"), true, false, false);
		else
			event = ItemFactory.newEventsSelector("event", I18N.message("event"), false, false, false);
		event.setEndRow(true);
		event.setDisabled(true);

		notifyon.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent e) {
				if ("selection".equals(form.getValueAsString("notifyon")))
					event.setDisabled(false);
				else
					event.setDisabled(true);
			}
		});

		ButtonItem subscribe = new ButtonItem();
		subscribe.setTitle(I18N.message("subscribe"));
		subscribe.setAutoFit(true);
		subscribe.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				String[] events = null;
				final String eventsStr;
				if ("selection".equals(form.getValueAsString("notifyon"))) {
					String buf = form.getValues().get("event").toString().trim().toLowerCase();
					buf = buf.replace('[', ' ');
					buf = buf.replace(']', ' ');
					eventsStr = buf.replace(" ", "");
					events = eventsStr.split(",");
				} else
					eventsStr = null;

				if (folderId != null)
					AuditService.Instance.get().subscribeFolder(folderId,
							form.getValueAsString("option").equals("current"), events, null, null,
							new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("foldersubscribed"), null);
									Session.get().getUser()
											.setSubscriptions(Session.get().getUser().getSubscriptions() + 1);
								}
							});
				else
					AuditService.Instance.get().subscribeDocuments(docIds, events, null, null,
							new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("documentsubscribed"), null);
									Session.get().getUser()
											.setSubscriptions(Session.get().getUser().getSubscriptions() + 1);
								}
							});
				destroy();
			}
		});

		if (folderId != null)
			form.setItems(option, notifyon, event, subscribe);
		else
			form.setItems(notifyon, event, subscribe);
		addItem(form);
	}
}
