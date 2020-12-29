package com.logicaldoc.gui.frontend.client.calendar;

import java.util.Date;
import java.util.LinkedHashMap;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This is the form used for editing a calendar event.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class CalendarEventDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private GUICalendarEvent calendarEvent;

	private TabSet tabs = new TabSet();

	private DynamicForm detailsForm = new DynamicForm();

	private boolean readOnly = false;

	private AsyncCallback<Void> onChangedCallback;

	public CalendarEventDialog(GUICalendarEvent calEvent, AsyncCallback<Void> onChangedCallback) {
		this.calendarEvent = calEvent;
		this.onChangedCallback = onChangedCallback;

		readOnly = Session.get().getUser().getId() != calEvent.getCreatorId()
				&& !Session.get().getUser().isMemberOf("admin");

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (calEvent.getId() != 0)
			setTitle(I18N.message("editevent") + " - " + calEvent.getTitle());
		else
			setTitle(I18N.message("newevent"));
		setWidth(530);
		setHeight(500);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);

		Tab detailsTab = prepareDetailsTab();
		Tab participantsTab = prepareParticipants();
		Tab documentsTab = prepareDocuments();

		tabs.setTabs(detailsTab, participantsTab, documentsTab);
		tabs.setHeight100();
		addItem(tabs);

		HLayout buttonsPanel = new HLayout();

		IButton save = new IButton();
		save.setMargin(3);
		save.setHeight(30);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		IButton delete = new IButton();
		delete.setMargin(3);
		delete.setHeight(30);
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onDelete();
			}
		});

		if (calendarEvent.getId() != 0)
			buttonsPanel.setMembers(save, delete);
		else
			buttonsPanel.setMembers(save);

		if (!readOnly)
			addItem(buttonsPanel);
	}

	private Tab prepareParticipants() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);
		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth("*");
		ListGridField username = new ListGridField("username", I18N.message("username"));
		username.setWidth(110);

		final ListGrid list = new ListGrid();
		list.setHeight100();
		list.setWidth100();
		list.setFields(id, username, name);

		ListGridRecord[] records = new ListGridRecord[calendarEvent.getParticipants().length];
		for (int i = 0; i < calendarEvent.getParticipants().length; i++) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("id", calendarEvent.getParticipants()[i].getId());
			records[i].setAttribute("name", calendarEvent.getParticipants()[i].getFullName());
			records[i].setAttribute("username", calendarEvent.getParticipants()[i].getUsername());
		}
		list.setRecords(records);

		Menu contextMenu = new Menu();
		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message("ddelete"));
		deleteItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				for (ListGridRecord rec : selection) {
					if (rec.getAttribute("id").startsWith("g-"))
						CalendarEventDialog.this.calendarEvent
								.removeParticipantGroup(Long.parseLong(rec.getAttribute("id").substring(2)));
					else
						CalendarEventDialog.this.calendarEvent.removeParticipant(rec.getAttributeAsLong("id"));
				}

				list.removeSelectedData();
			}
		});
		if (!readOnly) {
			contextMenu.setItems(deleteItem);
			list.setContextMenu(contextMenu);
		}

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		final SelectItem newUser = ItemFactory.newUserSelector("user", "adduser", null, true, true);
		newUser.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = newUser.getSelectedRecord();
				if (selectedRecord == null)
					return;

				String id = selectedRecord.getAttribute("id");
				String label = selectedRecord.getAttribute("label");
				String username = selectedRecord.getAttribute("username");

				addParticipant(list, id, username, label);
				newUser.clearValue();
			}
		});

		final SelectItem newGroup = ItemFactory.newGroupSelector("group", "addgroup");
		newGroup.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = newGroup.getSelectedRecord();
				if (selectedRecord == null)
					return;

				String id = "g-" + selectedRecord.getAttribute("id");
				String label = I18N.message("group") + ": " + selectedRecord.getAttribute("name");
				String username = selectedRecord.getAttribute("name");
				
				addParticipant(list, id, username, label);
				newGroup.clearValue();
			}
		});

		form.setItems(newUser, newGroup);

		Tab participantsTab = new Tab();
		participantsTab.setTitle(I18N.message("participants"));
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		if (readOnly)
			layout.setMembers(list);
		else
			layout.setMembers(list, form);
		participantsTab.setPane(layout);
		return participantsTab;
	}

	private Tab prepareDocuments() {
		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		fileName.setWidth("90%");
		fileName.setCanEdit(!readOnly);

		ListGridField lastModified = new ListGridField("lastModified", I18N.message("lastmodified"), 150);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		ListGridField icon = new ListGridField("icon", " ", 24);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(icon, fileName, lastModified);
		refreshDocumentsGrid(list);

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				// Detect the selected record
				ListGridRecord selection = list.getSelectedRecord();

				long id = Long.parseLong(selection.getAttribute("id"));

				DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught.getMessage(), caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				// Detect selected records
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection != null && selection.length > 0) {
					for (ListGridRecord record : selection) {
						calendarEvent.removeDocument(Long.parseLong(record.getAttribute("id")));
					}
					refreshDocumentsGrid(list);
				}
			}
		});

		Menu contextMenu = new Menu();
		if (!readOnly)
			contextMenu.setItems(preview, delete);
		else
			contextMenu.setItems(preview);
		list.setContextMenu(contextMenu);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				destroy();
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("folderId")),
						Long.parseLong(record.getAttributeAsString("id")));
			}
		});

		IButton addDocuments = new IButton();
		addDocuments.setTitle(I18N.message("adddocuments"));
		addDocuments.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				Clipboard clipboard = Clipboard.getInstance();
				if (clipboard.isEmpty()) {
					SC.warn(I18N.message("nodocsinclipboard"));
					return;
				}

				for (GUIDocument doc : clipboard) {
					calendarEvent.addDocument(doc);
				}
				Clipboard.getInstance().clear();
				refreshDocumentsGrid(list);
			}
		});

		Tab documentsTab = new Tab();
		documentsTab.setTitle(I18N.message("documents"));
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setAlign(Alignment.CENTER);
		layout.setMembersMargin(3);

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.RIGHT);
		buttons.setMembers(addDocuments);

		if (!readOnly)
			layout.setMembers(list, buttons);
		else
			layout.setMembers(list);

		documentsTab.setPane(layout);
		return documentsTab;
	}

	private void refreshDocumentsGrid(final ListGrid list) {
		ListGridRecord[] records = new ListGridRecord[calendarEvent.getDocuments().length];
		for (int i = 0; i < calendarEvent.getDocuments().length; i++) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("id", calendarEvent.getDocuments()[i].getId());
			records[i].setAttribute("icon", calendarEvent.getDocuments()[i].getIcon());
			records[i].setAttribute("version", calendarEvent.getDocuments()[i].getVersion());
			records[i].setAttribute("fileVersion", calendarEvent.getDocuments()[i].getFileVersion());
			records[i].setAttribute("filename", calendarEvent.getDocuments()[i].getFileName());
			records[i].setAttribute("lastModified", calendarEvent.getDocuments()[i].getLastModified());
			records[i].setAttribute("docRef", calendarEvent.getDocuments()[i].getDocRef());
		}
		list.setRecords(records);
	}

	private Tab prepareDetailsTab() {
		Tab details = new Tab();
		details.setTitle(I18N.message("details"));

		final int formColumns = 6;
		detailsForm.setHeight100();
		detailsForm.setTitleOrientation(TitleOrientation.TOP);
		detailsForm.setNumCols(formColumns);
		detailsForm.setValuesManager(vm);

		TextItem title = ItemFactory.newTextItem("title", "title", calendarEvent.getTitle());
		title.setRequired(true);
		title.setEndRow(true);
		title.setWrapTitle(false);
		title.setColSpan(5);
		title.setWidth(350);
		title.setTitleOrientation(TitleOrientation.LEFT);
		title.setLength(255);
		title.setCanEdit(!readOnly);

		TextItem type = ItemFactory.newTextItem("type", "type", calendarEvent.getType());
		type.setRequired(false);
		type.setEndRow(true);
		type.setWrapTitle(false);
		type.setColSpan(5);
		type.setWidth(350);
		type.setTitleOrientation(TitleOrientation.LEFT);
		type.setLength(255);
		type.setCanEdit(!readOnly);

		TextItem subType = ItemFactory.newTextItem("subType", "subtype", calendarEvent.getSubType());
		subType.setRequired(false);
		subType.setEndRow(true);
		subType.setWrapTitle(false);
		subType.setColSpan(5);
		subType.setWidth(350);
		subType.setTitleOrientation(TitleOrientation.LEFT);
		subType.setLength(255);
		subType.setCanEdit(!readOnly);

		DateItem startDate = ItemFactory.newDateItem("startDate", "begin");
		startDate.setRequired(true);
		startDate.setTitleOrientation(TitleOrientation.LEFT);
		startDate.setWrapTitle(false);
		startDate.setValue(calendarEvent.getStartDate());
		startDate.setCanEdit(!readOnly);
		TimeItem startTime = ItemFactory.newTimeItem("startTime", "   ");
		DateTimeFormat df = DateTimeFormat.getFormat("HH:mm");
		startTime.setValue(df.format(calendarEvent.getStartDate()));
		startTime.setRequired(true);
		startTime.setShowTitle(false);
		startTime.setTitleOrientation(TitleOrientation.LEFT);
		startTime.setEndRow(true);
		startTime.setCanEdit(!readOnly);
		startTime.setTitleColSpan(1);

		DateItem expirationDate = ItemFactory.newDateItem("expirationDate", "expirationdate");
		expirationDate.setRequired(false);
		expirationDate.setTitleOrientation(TitleOrientation.LEFT);
		expirationDate.setWrapTitle(false);
		expirationDate.setCanEdit(!readOnly);
		if (calendarEvent.getExpirationDate() != null)
			expirationDate.setValue(calendarEvent.getExpirationDate());
		TimeItem expirationTime = ItemFactory.newTimeItem("expirationTime", "   ");
		expirationTime.setTitleOrientation(TitleOrientation.LEFT);
		expirationTime.setShowTitle(false);
		expirationTime.setEndRow(true);
		expirationTime.setCanEdit(!readOnly);
		expirationTime.setTitleColSpan(1);
		if (calendarEvent.getExpirationDate() != null)
			expirationTime.setValue(df.format(calendarEvent.getExpirationDate()));

		final DateItem deadline = ItemFactory.newDateItem("deadline", "enddate");
		deadline.setRequired(false);
		deadline.setShowTitle(true);
		deadline.setTitleOrientation(TitleOrientation.LEFT);
		deadline.setWrapTitle(false);
		deadline.setCanEdit(!readOnly);
		if (calendarEvent.getDeadline() != null)
			deadline.setValue(calendarEvent.getDeadline());

		final SelectItem frequency = ItemFactory.newFrequencySelector("frequency", "frequency");
		frequency.setTitleOrientation(TitleOrientation.LEFT);
		frequency.setValue(Integer.toString(calendarEvent.getFrequency()));
		frequency.setCanEdit(!readOnly);
		frequency.setWrapTitle(false);
		frequency.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				deadline.setDisabled("0".equals(event.getValue()));
				if ("0".equals(event.getValue()))
					deadline.setValue((Date) null);
			}
		});

		SpinnerItem remindTimeNumber = new SpinnerItem("remindTime");
		remindTimeNumber.setTitle(I18N.message("remindtime"));
		remindTimeNumber.setTitleOrientation(TitleOrientation.LEFT);
		remindTimeNumber.setWrapTitle(false);
		remindTimeNumber.setDefaultValue(0);
		remindTimeNumber.setMin(0);
		remindTimeNumber.setStep(1);
		remindTimeNumber.setWidth(50);
		remindTimeNumber.setValue(calendarEvent.getRemindTime());
		remindTimeNumber.setCanEdit(!readOnly);
		SelectItem remindTimeUnit = ItemFactory.newDueTimeSelector("remindUnit", "");
		remindTimeUnit.setShowTitle(false);
		remindTimeUnit.setWrapTitle(false);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("minute", I18N.message("minutes"));
		map.put("hour", I18N.message("hours"));
		map.put("day", I18N.message("ddays"));
		remindTimeUnit.setValueMap(map);
		remindTimeUnit.setValue(calendarEvent.getRemindUnit());
		remindTimeUnit.setColSpan(3);
		remindTimeUnit.setAlign(Alignment.LEFT);
		remindTimeUnit.setCanEdit(!readOnly);

		final DateItem completionDate = ItemFactory.newDateItem("completionDate", "completedon");
		completionDate.setRequired(false);
		completionDate.setShowTitle(false);
		completionDate.setTitleOrientation(TitleOrientation.LEFT);
		completionDate.setCanEdit(!readOnly);
		completionDate.setDisabled(calendarEvent.getStatus() != GUICalendarEvent.STATUS_COMPLETED);
		if (calendarEvent.getCompletionDate() != null)
			completionDate.setValue(calendarEvent.getCompletionDate());

		SelectItem status = ItemFactory.newCalendarEventStatusSelector("status", "status");
		status.setTitleOrientation(TitleOrientation.LEFT);
		status.setWrapTitle(false);
		status.setValue(Integer.toString(calendarEvent.getStatus()));
		status.setCanEdit(!readOnly);
		status.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				completionDate.setDisabled(!"2".equals(event.getValue()));
				if ("2".equals(event.getValue()))
					completionDate.setValue(new Date());
				else
					completionDate.setValue((Date) null);
			}
		});

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description",
				calendarEvent.getDescription());
		description.setWidth("*");
		description.setHeight("*");
		description.setColSpan(formColumns);
		description.setCanEdit(!readOnly);

		detailsForm.setFields(title, type, subType, ItemFactory.newRowSpacer(), startDate, startTime, expirationDate,
				expirationTime, ItemFactory.newRowSpacer(), frequency, deadline, ItemFactory.newRowSpacer(),
				remindTimeNumber, remindTimeUnit, ItemFactory.newRowSpacer(), status, completionDate,
				ItemFactory.newRowSpacer(), description);
		details.setPane(detailsForm);
		return details;
	}

	/**
	 * Save button handler
	 */
	private void onSave() {
		if (vm.validate()) {
			if (calendarEvent.getDocuments() == null || calendarEvent.getDocuments().length == 0) {
				SC.warn(I18N.message("nodocsincalevent"));
				return;
			}

			calendarEvent.setTitle(vm.getValueAsString("title"));
			calendarEvent.setType(vm.getValueAsString("type"));
			calendarEvent.setSubType(vm.getValueAsString("subType"));
			calendarEvent.setDescription(vm.getValueAsString("description"));
			calendarEvent.setRemindTime(Integer.parseInt(vm.getValueAsString("remindTime")));
			calendarEvent.setRemindUnit(vm.getValueAsString("remindUnit"));

			if (vm.getValue("frequency") != null)
				calendarEvent.setFrequency(Integer.parseInt(vm.getValueAsString("frequency").trim()));

			DateTimeFormat dfDate = DateTimeFormat.getFormat("yyyy-MM-dd");
			DateTimeFormat dfTime = DateTimeFormat.getFormat("HH:mm");
			DateTimeFormat df = DateTimeFormat.getFormat("yyyy-MM-dd HH:mm");

			String str = dfDate.format((Date) vm.getValue("startDate"));
			if (vm.getValue("startTime") != null)
				try {
					calendarEvent.setStartDate(df.parse(str + " " + vm.getValue("startTime").toString()));
				} catch (Throwable t) {
					calendarEvent.setStartDate(df.parse(str + " " + dfTime.format((Date) vm.getValue("startTime"))));
				}

			if (vm.getValue("expirationDate") != null) {
				str = dfDate.format((Date) vm.getValue("expirationDate"));
				if (vm.getValue("expirationTime") != null)
					try {
						calendarEvent.setExpirationDate(df.parse(str + " " + vm.getValue("expirationTime").toString()));
					} catch (Throwable t) {
						calendarEvent.setExpirationDate(
								df.parse(str + " " + dfTime.format((Date) vm.getValue("expirationTime"))));
					}
			}

			if (calendarEvent.getExpirationDate() != null
					&& calendarEvent.getExpirationDate().before(calendarEvent.getStartDate())) {
				SC.warn(I18N.message("endbeforestart"));
				return;
			}

			if (vm.getValue("completionDate") != null)
				calendarEvent.setCompletionDate((Date) vm.getValue("completionDate"));
			else
				calendarEvent.setCompletionDate(null);

			calendarEvent.setStatus(Integer.parseInt(vm.getValue("status").toString()));

			if (vm.getValue("deadline") != null)
				calendarEvent.setDeadline((Date) vm.getValue("deadline"));
			else
				calendarEvent.setDeadline(null);

			CalendarService.Instance.get().saveEvent(calendarEvent, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					destroy();
					if (onChangedCallback != null)
						onChangedCallback.onSuccess(arg);
				}
			});
		}
	}

	/**
	 * Delete button handler
	 */
	private void onDelete() {
		GUIUser currentUser = Session.get().getUser();
		if (currentUser.getId() != calendarEvent.getCreatorId() && !currentUser.isMemberOf("admin")) {
			return;
		}

		LD.ask(I18N.message("delevent"), I18N.message("deleventconfirm"), new BooleanCallback() {
			@Override
			public void execute(Boolean value) {
				if (value.booleanValue()) {
					if (calendarEvent.getParentId() != null) {
						LD.ask(I18N.message("delevent"), I18N.message("douwantdeletealloccurrences"),
								new BooleanCallback() {
									@Override
									public void execute(Boolean value) {
										Long id = value ? calendarEvent.getParentId() : calendarEvent.getId();
										CalendarService.Instance.get().deleteEvent(id, new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable caught) {
												Log.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												destroy();
											}
										});
									}
								});
					} else
						CalendarService.Instance.get().deleteEvent(calendarEvent.getId(), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								destroy();
								if (onChangedCallback != null)
									onChangedCallback.onSuccess(arg);
							}
						});
				}
			}
		});
	}

	private void addParticipant(final ListGrid list, String id, String username, String name) {
		// Check if the selected user is already present in the list
		ListGridRecord[] records = list.getRecords();
		for (ListGridRecord test : records) {
			if (test.getAttribute("id").equals(id))
				return;
		}

		// Update the table
		ListGridRecord record = new ListGridRecord();

		record.setAttribute("id", id);
		record.setAttribute("name", name);
		record.setAttribute("username", username);
		list.addData(record);
		
		if (id.startsWith("g-")) {
			GUIGroup group = new GUIGroup();
			group.setId(Long.parseLong(id.substring(2)));
			group.setName(username);
			group.setDescription(name);
			CalendarEventDialog.this.calendarEvent.addParticipant(group);
		} else {
			GUIUser user = new GUIUser();
			user.setId(Long.parseLong(id));
			user.setUsername(username);
			user.setFirstName(name);
			CalendarEventDialog.this.calendarEvent.addParticipant(user);
		}
	}
}