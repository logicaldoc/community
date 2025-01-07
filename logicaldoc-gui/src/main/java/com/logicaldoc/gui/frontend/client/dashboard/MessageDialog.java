package com.logicaldoc.gui.frontend.client.dashboard;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.EventPanel;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This is the form used to send messages to other users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessageDialog extends Window {

	private static final String AVATAR = "avatar";

	private static final String LABEL = "label";

	private static final String PRIORITY = "priority";

	private static final String VALIDITY = "validity";

	private static final String CONFIRMATION = "confirmation";

	private DynamicForm form = new DynamicForm();

	private ListGrid recipientsGrid;

	public MessageDialog() {
		super();

		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sendmessage"));
		setWidth(580);
		setHeight(590);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(false);

		int formColumns = 2;
		form.setWidth100();
		form.setMargin(3);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(formColumns);

		TextItem subject = ItemFactory.newTextItem("subject", "");
		subject.setRequired(true);
		subject.setBrowserSpellCheck(true);
		subject.setWidth("*");
		subject.setColSpan(formColumns);

		final TextAreaItem message = ItemFactory.newTextAreaItem("message", null);
		message.setHeight(160);
		message.setBrowserSpellCheck(true);
		message.setWidth("*");
		message.setColSpan(formColumns);

		final CheckboxItem confirmation = new CheckboxItem();
		confirmation.setName(CONFIRMATION);
		confirmation.setTitle(I18N.message(CONFIRMATION));
		confirmation.setEndRow(true);

		IntegerItem validity = ItemFactory.newIntegerItem(VALIDITY, I18N.message(VALIDITY), 1);
		IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
		integerRangeValidator.setMin(1);
		validity.setValidators(integerRangeValidator);
		validity.setHint(I18N.message("days"));

		SelectItem priority = ItemFactory.newPrioritySelector(PRIORITY, I18N.message(PRIORITY));

		final IButton send = new IButton();
		send.setTitle(I18N.message("submit"));
		send.setAutoFit(true);
		send.setMargin(3);
		send.addClickHandler(event -> {
			form.validate();
			if (Boolean.FALSE.equals(form.hasErrors())) {
				GUIMessage msg = new GUIMessage();
				msg.setSubject(form.getValueAsString("subject"));
				msg.setMessage(form.getValueAsString("message"));
				msg.setConfirmation(Boolean.valueOf(form.getValueAsString(CONFIRMATION)));
				if (form.getValueAsString(VALIDITY) != null)
					msg.setValidity(Integer.parseInt(form.getValueAsString(VALIDITY)));
				msg.setPriority(Integer.parseInt(form.getValue(PRIORITY).toString()));

				final ListGridRecord[] records = recipientsGrid.getRecords();
				if (records == null || records.length < 1) {
					SC.warn(I18N.message("noselectedrecipients"));
					return;
				}

				MessageService.Instance.get().save(msg, GridUtil.getIds(records), new DefaultAsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(Void result) {
						EventPanel.get().info(I18N.message("messagesent"), null);
						destroy();
					}
				});
			}
		});
		form.setFields(subject, confirmation, validity, priority, message);

		HLayout buttons = new HLayout();
		buttons.setMembers(send);
		buttons.setHeight(30);
		buttons.setWidth100();

		SectionStack recipientsStack = prepareRecipientsGrid();

		addItem(recipientsStack);
		addItem(form);
		addItem(buttons);
	}

	private SectionStack prepareRecipientsGrid() {
		SectionStack sectionStack = new SectionStack();
		sectionStack.setWidth100();
		sectionStack.setHeight(200);
		sectionStack.setMargin(6);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("recipients") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		UserListGridField avatar = new UserListGridField();

		ListGridField name = new ListGridField(LABEL, I18N.message("name"));
		name.setCanFilter(true);

		ListGridField id = new ListGridField("id", I18N.message(" "));
		id.setWidth(50);
		id.setHidden(true);

		recipientsGrid = new ListGrid();
		recipientsGrid.setShowRecordComponents(true);
		recipientsGrid.setShowRecordComponentsByCell(true);
		recipientsGrid.setAutoFetchData(true);
		recipientsGrid.setCanEdit(false);
		recipientsGrid.setShowHeader(false);
		recipientsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		recipientsGrid.setWidth100();
		recipientsGrid.setEditEvent(ListGridEditEvent.CLICK);
		recipientsGrid.setFields(id, avatar, name);

		recipientsGrid.addCellContextClickHandler(event -> {
			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.addClickHandler(evt -> recipientsGrid.removeSelectedData());

			Menu contextMenu = new Menu();
			contextMenu.setItems(delete);
			contextMenu.showContextMenu();

			event.cancel();
		});

		final SelectItem userSelector = ItemFactory.newUserSelector("users", "users", null, false, true);
		userSelector.setWidth(150);
		userSelector.setMultiple(true);
		userSelector.addChangedHandler(event -> {
			addRecipients(Arrays.asList(userSelector.getSelectedRecords()));
			userSelector.clearValue();
		});

		final SelectItem groupSelector = ItemFactory.newGroupSelector("groups", "groups");
		groupSelector.setWidth(150);
		groupSelector.setMultiple(false);
		groupSelector.addChangedHandler(event -> {
			String groupId = groupSelector.getSelectedRecord().getAttributeAsString("id");
			SecurityService.Instance.get().searchUsers(null, groupId, new DefaultAsyncCallback<>() {

				@Override
				public void onSuccess(List<GUIUser> users) {
					if (users.isEmpty())
						return;

					List<ListGridRecord> records = new ArrayList<>();
					for (GUIUser user : users) {
						ListGridRecord rec = new ListGridRecord();
						rec.setAttribute("id", user.getId());
						rec.setAttribute("username", user.getUsername());
						rec.setAttribute(AVATAR, user.getId());
						rec.setAttribute(LABEL, user.getFullName());
						records.add(rec);
					}

					addRecipients(records);
					groupSelector.clearValue();
				}
			});
		});

		DynamicForm selectionForm = new DynamicForm();
		selectionForm.setNumCols(4);
		selectionForm.setItems(userSelector, groupSelector);

		section.setItems(recipientsGrid, selectionForm);
		sectionStack.setSections(section);

		return sectionStack;
	}

	/**
	 * Add new users in the recipients grid
	 */
	private void addRecipients(List<ListGridRecord> newSelection) {
		if (newSelection.isEmpty())
			return;

		for (ListGridRecord rcd : newSelection) {
			ListGridRecord newRec = new ListGridRecord();
			newRec.setAttribute("id", rcd.getAttributeAsString("id"));
			newRec.setAttribute(AVATAR, rcd.getAttributeAsString(AVATAR));
			newRec.setAttribute(LABEL, rcd.getAttributeAsString(LABEL));

			// Iterate over the current recipients avoiding duplicates
			boolean duplicate = false;
			ListGridRecord[] currentRecipients = recipientsGrid.getRecords();
			for (int j = 0; j < currentRecipients.length; j++) {
				ListGridRecord rec = currentRecipients[j];
				if (rec.getAttributeAsString("id").equals(newRec.getAttributeAsString("id"))) {
					duplicate = true;
					break;
				}
			}

			if (!duplicate)
				recipientsGrid.addData(newRec);
		}
	}
}
