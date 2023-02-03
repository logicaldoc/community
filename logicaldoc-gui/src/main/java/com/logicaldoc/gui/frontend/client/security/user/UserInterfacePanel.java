package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Panel to edit the User Interface related settings of a user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class UserInterfacePanel extends HLayout {
	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private ChangedHandler changedHandler;

	private VLayout layout = new VLayout();

	public UserInterfacePanel(GUIUser user, ChangedHandler changedHandler) {
		if (user == null) {
			setMembers(UsersPanel.SELECT_USER);
		} else {
			this.user = user;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);
			layout.setWidth(300);

			prepareGUI();
		}
	}

	public void prepareGUI() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);

		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setWrapItemTitles(false);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setNumCols(2);

		layout.addMember(form1, 1);

		SelectItem welcomeScreen = prepareWelcomeScreenSelector(readonly);

		SelectItem defaultWorkspace = prepareDefaultWorkspaceSelectot(readonly);

		PickerIcon clearIcon = prepareClearPickerIcon(readonly);

		StaticTextItem docsGrid = prepreDocsGridLayoutItem(readonly, clearIcon);

		StaticTextItem hitsGrid = prepareHitsGridLayoutItem(readonly, clearIcon);

		TextItem dateFormat = ItemFactory.newTextItem("dateformat", user.getDateFormat());
		TextItem dateFormatShort = ItemFactory.newTextItem("dateformatshort", user.getDateFormatShort());
		TextItem dateFormatLong = ItemFactory.newTextItem("dateformatlong", user.getDateFormatLong());

		form1.setItems(welcomeScreen, defaultWorkspace, docsGrid, hitsGrid, dateFormat, dateFormatShort,
				dateFormatLong);

		addMember(layout);
	}

	private SelectItem prepareDefaultWorkspaceSelectot(boolean readonly) {
		SelectItem defaultWorkspace = ItemFactory.newWorkspaceSelector(user.getDefaultWorkspace());
		defaultWorkspace.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		if (!readonly)
			defaultWorkspace.addChangedHandler(changedHandler);
		return defaultWorkspace;
	}

	private SelectItem prepareWelcomeScreenSelector(boolean readonly) {
		SelectItem welcomeScreen = ItemFactory.newWelcomeScreenSelector(user.getWelcomeScreen());
		welcomeScreen.setDisabled(readonly || (Session.get().isDemo() && Session.get().getUser().getId() == 1));
		if (!readonly)
			welcomeScreen.addChangedHandler(changedHandler);
		return welcomeScreen;
	}

	private StaticTextItem prepreDocsGridLayoutItem(boolean readonly, PickerIcon clearIcon) {
		StaticTextItem docsGrid = ItemFactory.newStaticTextItem("docsgrid", "docsgridlayout",
				user.getDocsGrid() != null && !user.getDocsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message("notcustomized"));
		FormItemIcon editDocsLayout = new FormItemIcon();
		editDocsLayout.setSrc("[SKIN]/paste.gif");
		editDocsLayout.setPrompt(I18N.message("editlayout"));
		editDocsLayout.setWidth(12);
		editDocsLayout.setHeight(12);
		editDocsLayout.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem("docsgridlayout", I18N.message("docsgridlayout"), null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message("docsgridlayout"), I18N.message("docsgridlayout"),
					user.getDocsGrid() != null ? user.getDocsGrid() : "", textArea, 400, new ValueCallback() {
						@Override
						public void execute(final String value) {
							if (!readonly) {
								user.setDocsGrid(value);
								changedHandler.onChanged(null);
								docsGrid.setValue(value != null && !value.isEmpty() ? I18N.message("customized")
										: I18N.message("notcustomized"));
							}
						}
					});
			event.cancel();
		});
		docsGrid.setIcons(editDocsLayout, clearIcon);
		return docsGrid;
	}

	private StaticTextItem prepareHitsGridLayoutItem(boolean readonly, PickerIcon clearIcon) {
		StaticTextItem hitsGrid = ItemFactory.newStaticTextItem("hitsgrid", "hitsgridlayout",
				user.getHitsGrid() != null && !user.getHitsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message("notcustomized"));
		FormItemIcon editHitsLayout = new FormItemIcon();
		editHitsLayout.setSrc("[SKIN]/paste.gif");
		editHitsLayout.setPrompt(I18N.message("editlayout"));
		editHitsLayout.setWidth(12);
		editHitsLayout.setHeight(12);
		editHitsLayout.addFormItemClickHandler(new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				TextAreaItem textArea = ItemFactory.newTextAreaItem("hitsgridlayout", I18N.message("hitsgridlayout"),
						null);
				textArea.setHeight(300);
				LD.askForValue(I18N.message("hitsgridlayout"), I18N.message("hitsgridlayout"),
						user.getHitsGrid() != null ? user.getHitsGrid() : "", textArea, 400, (final String value) -> {
							if (!readonly) {
								user.setHitsGrid(value);
								changedHandler.onChanged(null);
								hitsGrid.setValue(value != null && !value.isEmpty() ? I18N.message("customized")
										: I18N.message("notcustomized"));
							}
						});
				event.cancel();
			}
		});
		hitsGrid.setIcons(editHitsLayout, clearIcon);
		return hitsGrid;
	}

	private PickerIcon prepareClearPickerIcon(boolean readonly) {
		PickerIcon clearIcon = new PickerIcon(PickerIcon.CLEAR, (FormItemIconClickEvent event) -> {
			event.getItem().setValue(I18N.message("notcustomized"));
			changedHandler.onChanged(null);
		});
		clearIcon.setWidth(12);
		clearIcon.setHeight(12);
		clearIcon.setDisabled(readonly);
		return clearIcon;
	}

	boolean validate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return false;

		collectValues();

		return !vm.hasErrors();
	}

	private void collectValues() {
		user.setWelcomeScreen(Integer.parseInt(vm.getValueAsString("welcomescreen")));
		String str = vm.getValueAsString("workspace");
		if (str != null && !str.isEmpty())
			user.setDefaultWorkspace(Long.parseLong(str));

		if (vm.getValueAsString("docsgrid") == null || vm.getValueAsString("docsgrid").isEmpty()
				|| I18N.message("notcustomized").equals(vm.getValueAsString("docsgrid")))
			user.setDocsGrid(null);
		if (vm.getValueAsString("hitsgrid") == null || vm.getValueAsString("hitsgrid").isEmpty()
				|| I18N.message("notcustomized").equals(vm.getValueAsString("hitsgrid")))
			user.setHitsGrid(null);

		if (vm.getValueAsString("dateformat") == null || vm.getValueAsString("dateformat").isEmpty())
			user.setDateFormat(null);
		else
			user.setDateFormat(vm.getValueAsString("dateformat"));

		if (vm.getValueAsString("dateformatshort") == null || vm.getValueAsString("dateformatshort").isEmpty())
			user.setDateFormatShort(null);
		else
			user.setDateFormatShort(vm.getValueAsString("dateformatshort"));

		if (vm.getValueAsString("dateformatlong") == null || vm.getValueAsString("dateformatlong").isEmpty())
			user.setDateFormatLong(null);
		else
			user.setDateFormatLong(vm.getValueAsString("dateformatlong"));
	}
}