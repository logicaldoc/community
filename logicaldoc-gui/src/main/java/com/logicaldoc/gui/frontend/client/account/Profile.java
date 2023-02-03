package com.logicaldoc.gui.frontend.client.account;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.Avatar;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to change file data of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Profile extends Window {

	private ValuesManager vm = new ValuesManager();

	private ListGrid searchesList;

	public Profile(final GUIUser user) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("profile"));
		setWidth(600);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setCanDragResize(true);

		final DynamicForm detailsForm = new DynamicForm();
		detailsForm.setHeight100();
		detailsForm.setWidth(1);
		detailsForm.setValuesManager(vm);
		detailsForm.setMargin(5);
		detailsForm.setNumCols(3);
		detailsForm.setTitleOrientation(TitleOrientation.TOP);

		TextItem firstName = ItemFactory.newTextItem("firstname", user.getFirstName());
		firstName.setRequired(true);
		TextItem lastName = ItemFactory.newTextItem("lastname", user.getName());
		lastName.setRequired(true);
		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setValue(user.getLanguage());
		language.setDisabled(Session.get().isDemo() && Session.get().getUser().getId() == 1);

		TextItem address = ItemFactory.newTextItem("address", user.getAddress());
		TextItem postalCode = ItemFactory.newTextItem("postalcode", user.getPostalCode());
		TextItem city = ItemFactory.newTextItem("city", user.getCity());
		TextItem country = ItemFactory.newTextItem("country", user.getCountry());
		TextItem state = ItemFactory.newTextItem("state", user.getState());
		TextItem phone = ItemFactory.newTextItem("phone", user.getPhone());
		TextItem cell = ItemFactory.newTextItem("cell", user.getCell());
		ComboBoxItem timeZone = ItemFactory.newTimeZoneSelector("timezone", "timezone", user.getTimeZone());
		timeZone.setEndRow(true);

		StaticTextItem quota = ItemFactory.newStaticTextItem("quota", "maxquota", Util.formatSizeW7(user.getQuota()));
		quota.setWrap(false);

		StaticTextItem quotaCount = ItemFactory.newStaticTextItem("quotaCount", "quota",
				Util.formatSizeW7(user.getQuotaCount()));
		quotaCount.setWrap(false);

		detailsForm.setFields(firstName, lastName, language, address, postalCode, city, country, state, phone, cell,
				timeZone, quotaCount, quota);

		HLayout detailsPanel = new HLayout();
		detailsPanel.setMembers(detailsForm, new Avatar(user.getId()));

		final DynamicForm emailForm = new DynamicForm();
		emailForm.setHeight100();
		emailForm.setValuesManager(vm);
		emailForm.setMargin(5);
		emailForm.setTitleOrientation(TitleOrientation.TOP);

		TextItem email = ItemFactory.newEmailItem("email", "email", false);
		email.setRequired(true);
		email.setWidth(300);
		email.setValue(user.getEmail());

		RichTextItem signature = new RichTextItem();
		signature.setName("signature");
		signature.setTitle(I18N.message("signature"));
		signature.setShowTitle(true);
		signature.setValue(user.getEmailSignature());
		signature.setWidth(getWidth() - 60);
		signature.setHeight(180);

		emailForm.setFields(email, signature);

		final DynamicForm emailForm2 = new DynamicForm();
		emailForm2.setHeight100();
		emailForm2.setValuesManager(vm);
		emailForm2.setMargin(5);
		emailForm2.setTitleOrientation(TitleOrientation.TOP);

		TextItem email2 = ItemFactory.newEmailItem("email2", "secondaryemail", false);
		email2.setWidth(300);
		email2.setValue(user.getEmail2());

		RichTextItem signature2 = new RichTextItem();
		signature2.setName("signature2");
		signature2.setTitle(I18N.message("signature"));
		signature2.setShowTitle(true);
		signature2.setValue(user.getEmailSignature2());
		signature2.setWidth(getWidth() - 60);
		signature2.setHeight(180);

		emailForm2.setFields(email2, signature2);

		SelectItem welcomeScreen = ItemFactory.newWelcomeScreenSelector(user.getWelcomeScreen());
		SelectItem defaultWorkspace = ItemFactory.newWorkspaceSelector(user.getDefaultWorkspace());

		PickerIcon clearDocsGrid = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				event.getItem().setValue(I18N.message("notcustomized"));
				user.setDocsGrid(null);
			}
		});
		clearDocsGrid.setWidth(12);
		clearDocsGrid.setHeight(12);

		PickerIcon clearHitsGrid = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				event.getItem().setValue(I18N.message("notcustomized"));
				user.setHitsGrid(null);
			}
		});
		clearHitsGrid.setWidth(12);
		clearHitsGrid.setHeight(12);

		FormItemIcon showDocsDefinition = new FormItemIcon();
		showDocsDefinition.setSrc("[SKIN]/paste.gif");
		showDocsDefinition.setPrompt(I18N.message("editlayout"));
		showDocsDefinition.setWidth(12);
		showDocsDefinition.setHeight(12);
		showDocsDefinition.addFormItemClickHandler(new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				TextAreaItem textArea = ItemFactory.newTextAreaItem("docsgridlayout", I18N.message("docsgridlayout"),
						null);
				textArea.setHeight(300);
				LD.askForValue(I18N.message("docsgridlayout"), I18N.message("docsgridlayout"),
						user.getDocsGrid() != null ? user.getDocsGrid() : "", textArea, 400, new ValueCallback() {
							@Override
							public void execute(final String value) {
								user.setDocsGrid(value);
							}
						});
				event.cancel();
			}
		});

		FormItemIcon showHitsDefinition = new FormItemIcon();
		showHitsDefinition.setSrc("[SKIN]/paste.gif");
		showHitsDefinition.setPrompt(I18N.message("editlayout"));
		showHitsDefinition.setWidth(12);
		showHitsDefinition.setHeight(12);
		showHitsDefinition.addFormItemClickHandler(new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				TextAreaItem textArea = ItemFactory.newTextAreaItem("hitsgridlayout", I18N.message("hitsgridlayout"),
						null);
				textArea.setHeight(300);
				LD.askForValue(I18N.message("hitsgridlayout"), I18N.message("hitsgridlayout"),
						user.getHitsGrid() != null ? user.getHitsGrid() : "", textArea, 400, new ValueCallback() {
							@Override
							public void execute(final String value) {
								user.setHitsGrid(value);
							}
						});
				event.cancel();
			}
		});

		StaticTextItem docsGrid = ItemFactory.newStaticTextItem("docsgrid", "docsgridlayout",
				user.getDocsGrid() != null && !user.getDocsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message("notcustomized"));
		docsGrid.setIcons(showDocsDefinition, clearDocsGrid);

		StaticTextItem hitsGrid = ItemFactory.newStaticTextItem("hitsgrid", "hitsgridlayout",
				user.getHitsGrid() != null && !user.getHitsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message("notcustomized"));
		hitsGrid.setIcons(showHitsDefinition, clearHitsGrid);

		TextItem dateFormat = ItemFactory.newTextItem("dateformat", user.getDateFormat());
		dateFormat.setWidth(180);
		TextItem dateFormatShort = ItemFactory.newTextItem("dateformatshort", user.getDateFormatShort());
		dateFormatShort.setWidth(180);
		TextItem dateFormatLong = ItemFactory.newTextItem("dateformatlong", user.getDateFormatLong());
		dateFormatLong.setWidth(180);

		final DynamicForm guiForm = new DynamicForm();
		guiForm.setHeight100();
		guiForm.setValuesManager(vm);
		guiForm.setMargin(5);
		guiForm.setTitleOrientation(TitleOrientation.TOP);

		guiForm.setFields(welcomeScreen, defaultWorkspace, docsGrid, hitsGrid, dateFormat, dateFormatShort,
				dateFormatLong);

		ArrayList<ListGridRecord> records = new ArrayList<ListGridRecord>();
		for (String search : user.orderedSearches()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("search", search);
			rec.setAttribute("label", I18N.message(search));
			records.add(rec);
		}

		ListGridField label = new ListGridField("label", I18N.message("searchpreference"));
		label.setWidth("*");
		label.setAllowFilterOperators(false);
		label.setCanFilter(false);
		label.setCanSort(false);

		searchesList = new ListGrid();
		searchesList.setCanReorderRecords(true);
		searchesList.setCanSort(false);
		searchesList.setCanSort(false);
		searchesList.setCanGroupBy(false);
		searchesList.setCanResizeFields(false);
		searchesList.setSelectionType(SelectionStyle.SINGLE);
		searchesList.setFields(label);
		searchesList.setData(records.toArray(new ListGridRecord[0]));

		HLayout guiLayout = new HLayout();
		guiLayout.setMembers(guiForm, searchesList);

		final TabSet tabs = new TabSet();
		tabs.setHeight100();
		tabs.setWidth100();

		Tab detailsTab = new Tab(I18N.message("details"));
		detailsTab.setPane(detailsPanel);

		Tab guiTab = new Tab(I18N.message("userinterface"));
		guiTab.setPane(guiLayout);

		Tab emailTab = new Tab(I18N.message("email"));
		emailTab.setPane(emailForm);
		Tab emailTab2 = new Tab(I18N.message("secondaryemail"));
		emailTab2.setPane(emailForm2);

		tabs.setTabs(detailsTab, guiTab, emailTab, emailTab2);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onSave(user, detailsForm, guiForm, emailForm, emailForm2, tabs);
			}
		});
		ToolStrip toolbar = new ToolStrip();
		toolbar.setWidth100();
		toolbar.setMembers(save);

		addItem(toolbar);
		addItem(tabs);
	}

	private void onSave(final GUIUser user, final DynamicForm detailsForm, final DynamicForm guiForm,
			final DynamicForm emailForm, final DynamicForm emailForm2, final TabSet tabs) {
		if (!validate(detailsForm, guiForm, emailForm, emailForm2, tabs))
			return;

		GUIUser u = new GUIUser();
		u.setId(user.getId());
		u.setFirstName(vm.getValueAsString("firstname"));
		u.setName(vm.getValueAsString("lastname"));
		u.setEmail(vm.getValueAsString("email"));
		u.setEmail2(vm.getValueAsString("email2"));
		u.setLanguage(vm.getValueAsString("language"));
		u.setAddress(vm.getValueAsString("address"));
		u.setPostalCode(vm.getValueAsString("postalcode"));
		u.setCity(vm.getValueAsString("city"));
		u.setCountry(vm.getValueAsString("country"));
		u.setState(vm.getValueAsString("state"));
		u.setPhone(vm.getValueAsString("phone"));
		u.setCell(vm.getValueAsString("cell"));
		u.setWelcomeScreen(Integer.parseInt(vm.getValueAsString("welcomescreen")));
		String str = vm.getValueAsString("workspace");
		if (str != null && !str.isEmpty())
			u.setDefaultWorkspace(Long.parseLong(str));
		u.setEmailSignature(vm.getValueAsString("signature"));
		u.setEmailSignature2(vm.getValueAsString("signature2"));
		u.setDocsGrid(user.getDocsGrid());
		u.setHitsGrid(user.getHitsGrid());
		u.setTimeZone(vm.getValueAsString("timezone"));

		if (vm.getValueAsString("dateformat") == null || vm.getValueAsString("dateformat").isEmpty())
			u.setDateFormat(null);
		else
			u.setDateFormat(vm.getValueAsString("dateformat"));

		if (vm.getValueAsString("dateformatshort") == null || vm.getValueAsString("dateformatshort").isEmpty())
			u.setDateFormatShort(null);
		else
			u.setDateFormatShort(vm.getValueAsString("dateformatshort"));

		if (vm.getValueAsString("dateformatlong") == null || vm.getValueAsString("dateformatlong").isEmpty())
			u.setDateFormatLong(null);
		else
			u.setDateFormatLong(vm.getValueAsString("dateformatlong"));

		ListGridRecord[] records = searchesList.getRecords();
		List<String> searches = new ArrayList<>();
		for (ListGridRecord rec : records)
			searches.add(rec.getAttributeAsString("search"));
		u.setSearchPref(searches.toString().replace("[", "").replace("]", "").replace(" ", ""));

		SecurityService.Instance.get().saveProfile(u, new AsyncCallback<GUIUser>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIUser ret) {
				// Update the currently logged user bean
				user.setFirstName(ret.getFirstName());
				user.setName(ret.getName());
				user.setEmail(ret.getEmail());
				user.setEmailSignature(ret.getEmailSignature());
				user.setEmail2(ret.getEmail2());
				user.setEmailSignature2(ret.getEmailSignature2());
				user.setLanguage(ret.getLanguage());
				user.setAddress(ret.getAddress());
				user.setPostalCode(ret.getPostalCode());
				user.setCity(ret.getCity());
				user.setCountry(ret.getCountry());
				user.setState(ret.getState());
				user.setPhone(ret.getPhone());
				user.setCell(ret.getCell());
				user.setWelcomeScreen(ret.getWelcomeScreen());
				user.setDefaultWorkspace(ret.getDefaultWorkspace());
				user.setDocsGrid(ret.getDocsGrid());
				user.setHitsGrid(ret.getHitsGrid());
				user.setDateFormat(ret.getDateFormat());
				user.setDateFormatShort(ret.getDateFormatShort());
				user.setDateFormatLong(ret.getDateFormatLong());
				user.setSearchPref(ret.getSearchPref());
				user.setTimeZone(ret.getTimeZone());

				Session.get().setUser(user);

				Profile.this.destroy();

				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private boolean validate(final DynamicForm detailsForm, final DynamicForm guiForm, final DynamicForm emailForm,
			final DynamicForm emailForm2, final TabSet tabs) {
		vm.validate();

		if (!detailsForm.validate())
			tabs.selectTab(0);
		else if (!guiForm.validate())
			tabs.selectTab(2);
		else if (!emailForm.validate())
			tabs.selectTab(2);
		else if (!emailForm2.validate())
			tabs.selectTab(3);

		return !vm.hasErrors();
	}
}
