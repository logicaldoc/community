package com.logicaldoc.gui.frontend.client.subscription;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This is the form used for subscribe a user to the events.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.5
 */
public class SubscriptionDialog extends Window {

	private static final String EVENT = "event";

	private static final String SELECTION = "selection";

	private static final String NOTIFYON = "notifyon";

	private static final String SUBFOLDERS = "subfolders";

	private static final String CURRENT = "current";

	private static final String OPTION = "option";

	public SubscriptionDialog(final ListGrid grid) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		ListGridRecord selection = grid.getSelectedRecord();

		setTitle(I18N.message("subscription") + " - " + selection.getAttributeAsString("name"));

		final String[] events = extractEvents(selection.getAttributeAsString("events"));

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

		SelectItem option = new SelectItem(OPTION, I18N.message("subscriptionoption"));
		option.setWidth(290);
		LinkedHashMap<String, String> options = new LinkedHashMap<>();
		options.put(CURRENT, I18N.message("subscribecurrent"));
		options.put(SUBFOLDERS, I18N.message("subscribesubfolders"));
		option.setValueMap(options);
		option.setValue("1".equals(selection.getAttributeAsString("folderOption")) ? SUBFOLDERS : CURRENT);

		SelectItem notifyon = new SelectItem(NOTIFYON, I18N.message(NOTIFYON));
		notifyon.setWidth(310);
		LinkedHashMap<String, String> vals = new LinkedHashMap<>();
		vals.put("all", I18N.message("allevents"));
		vals.put(SELECTION, I18N.message("selectedevents"));
		notifyon.setValueMap(vals);
		notifyon.setValue((events == null || events.length == 0) ? "all" : SELECTION);

		final SelectItem event;
		event = ItemFactory.newEventsSelector(EVENT, I18N.message(EVENT), null, true, false, false, false, false, false, false);
		event.setEndRow(true);
		event.setDisabled(events == null || events.length == 0);
		if (events != null)
			event.setValues(events);

		notifyon.addChangedHandler(
				(ChangedEvent e) -> event.setDisabled(!SELECTION.equals(form.getValueAsString(NOTIFYON))));

		ButtonItem save = prepareSaveButton(grid, form);

		if (isFolderSubscription)
			form.setItems(option, notifyon, event, save);
		else
			form.setItems(notifyon, event, save);

		addItem(form);
	}

	private String[] extractEvents(String input) {
		final String[] selectedEvents;
		if (input == null || input.isEmpty())
			selectedEvents = null;
		else if (!input.contains(","))
			selectedEvents = new String[] { input.trim() };
		else {
			selectedEvents = input.split(",");
		}
		return selectedEvents;
	}

	private List<Long> getSelectedIds(ListGrid grid) {
		ListGridRecord[] selectedRecords = grid.getSelectedRecords();
		List<Long> selectedIds = new ArrayList<>();
		for (int i = 0; i < selectedRecords.length; i++)
			selectedIds.add(selectedRecords[i].getAttributeAsLong("id"));
		return selectedIds;
	}

	private ButtonItem prepareSaveButton(ListGrid grid, DynamicForm form) {
		List<Long> selectedIds = getSelectedIds(grid);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> {
			List<String> events = new ArrayList<>();
			final String eventsStr;
			final String folderOption = form.getValueAsString(OPTION);
			if (SELECTION.equals(form.getValueAsString(NOTIFYON))) {
				String buf = form.getValues().get(EVENT).toString().trim().toLowerCase();
				buf = buf.replace('[', ' ');
				buf = buf.replace(']', ' ');
				eventsStr = buf.replace(" ", "");
				events.addAll(Arrays.asList(eventsStr.split(",")));
			} else
				eventsStr = null;

			doUpdateSubscriptions(grid, selectedIds, events, eventsStr, folderOption);
			destroy();
		});
		return save;
	}

	private void doUpdateSubscriptions(ListGrid grid, List<Long> selectedIds, List<String> events,
			final String eventsStr, final String folderOption) {
		AuditService.Instance.get().update(selectedIds, CURRENT.equals(folderOption), events,
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						GuiLog.info(I18N.message("settingssaved"), null);
						for (ListGridRecord rec : grid.getSelectedRecords()) {
							rec.setAttribute("events", eventsStr);
							if (folderOption != null && !folderOption.isEmpty())
								rec.setAttribute("folderOption", folderOption.equals(CURRENT) ? "0" : "1");
							grid.refreshRow(grid.getRecordIndex(rec));
						}
					}
				});
	}

	/**
	 * Constructor used to subscribe a user to folders or documents
	 * 
	 * @param folderId identifier of the folder
	 * @param docIds identifier of the documents
	 */
	public SubscriptionDialog(final Long folderId, List<Long> docIds) {
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

		SelectItem option = new SelectItem(OPTION, I18N.message("subscriptionoption"));
		option.setWidth(280);
		LinkedHashMap<String, String> options = new LinkedHashMap<>();
		options.put(CURRENT, I18N.message("subscribecurrent"));
		options.put(SUBFOLDERS, I18N.message("subscribesubfolders"));
		option.setValueMap(options);
		option.setValue(CURRENT);

		SelectItem notifyon = new SelectItem(NOTIFYON, I18N.message(NOTIFYON));
		notifyon.setWidth(280);
		LinkedHashMap<String, String> vals = new LinkedHashMap<>();
		vals.put("all", I18N.message("allevents"));
		vals.put(SELECTION, I18N.message("selectedevents"));
		notifyon.setValueMap(vals);
		notifyon.setValue("all");

		final SelectItem event = prepareEventSelector(folderId);

		notifyon.addChangedHandler(
				(ChangedEvent e) -> event.setDisabled(!SELECTION.equals(form.getValueAsString(NOTIFYON))));

		ButtonItem subscribe = prepareSubscribeButton(folderId, docIds, form);

		if (folderId != null)
			form.setItems(option, notifyon, event, subscribe);
		else
			form.setItems(notifyon, event, subscribe);
		addItem(form);
	}

	private ButtonItem prepareSubscribeButton(final Long folderId, List<Long> docIds, final DynamicForm form) {
		ButtonItem subscribe = new ButtonItem();
		subscribe.setTitle(I18N.message("subscribe"));
		subscribe.setAutoFit(true);
		subscribe.addClickHandler(event -> {
			List<String> events = new ArrayList<>();
			final String eventsStr;
			if (SELECTION.equals(form.getValueAsString(NOTIFYON))) {
				String buf = form.getValues().get(EVENT).toString().trim().toLowerCase();
				buf = buf.replace('[', ' ');
				buf = buf.replace(']', ' ');
				eventsStr = buf.replace(" ", "");
				events.addAll(Arrays.asList(eventsStr.split(",")));
			} else
				eventsStr = null;

			if (folderId != null)
				AuditService.Instance.get().subscribeFolder(folderId, form.getValueAsString(OPTION).equals(CURRENT),
						events, null, null, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								GuiLog.info(I18N.message("foldersubscribed"), null);
								Session.get().getUser()
										.setSubscriptions(Session.get().getUser().getSubscriptions() + 1);
							}
						});
			else
				AuditService.Instance.get().subscribeDocuments(docIds, events, null, null, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						GuiLog.info(I18N.message("documentsubscribed"), null);
						Session.get().getUser().setSubscriptions(Session.get().getUser().getSubscriptions() + 1);
					}
				});
			destroy();
		});
		return subscribe;
	}

	private SelectItem prepareEventSelector(final Long folderId) {
		final SelectItem event;
		if (folderId != null)
			event = ItemFactory.newEventsSelector(EVENT, EVENT, null, true, false, false, false, false, false, false);
		else
			event = ItemFactory.newEventsSelector(EVENT, EVENT, null, false, false, false, false, false, false, false);
		event.setEndRow(true);
		event.setDisabled(true);
		return event;
	}
}
