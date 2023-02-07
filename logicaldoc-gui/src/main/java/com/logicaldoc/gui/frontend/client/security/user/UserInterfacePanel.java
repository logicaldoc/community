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
	private static final String HITSGRID = "hitsgrid";

	private static final String HITSGRIDLAYOUT = "hitsgridlayout";

	private static final String NOTCUSTOMIZED = "notcustomized";

	private static final String CUSTOMIZED = "customized";

	private static final String DOCSGRIDLAYOUT = "docsgridlayout";

	private static final String DOCSGRID = "docsgrid";

	private static final String DATEFORMATLONG = "dateformatlong";

	private static final String DATEFORMATSHORT = "dateformatshort";

	private static final String DATEFORMAT = "dateformat";

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

		TextItem dateFormat = ItemFactory.newTextItem(DATEFORMAT, user.getDateFormat());
		TextItem dateFormatShort = ItemFactory.newTextItem(DATEFORMATSHORT, user.getDateFormatShort());
		TextItem dateFormatLong = ItemFactory.newTextItem(DATEFORMATLONG, user.getDateFormatLong());

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
		StaticTextItem docsGrid = ItemFactory.newStaticTextItem(DOCSGRID, DOCSGRIDLAYOUT,
				user.getDocsGrid() != null && !user.getDocsGrid().isEmpty() ? I18N.message(CUSTOMIZED)
						: I18N.message(NOTCUSTOMIZED));
		FormItemIcon editDocsLayout = new FormItemIcon();
		editDocsLayout.setSrc("[SKIN]/paste.gif");
		editDocsLayout.setPrompt(I18N.message("editlayout"));
		editDocsLayout.setWidth(12);
		editDocsLayout.setHeight(12);
		editDocsLayout.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem(DOCSGRIDLAYOUT, null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message(DOCSGRIDLAYOUT), I18N.message(DOCSGRIDLAYOUT),
					user.getDocsGrid() != null ? user.getDocsGrid() : "", textArea, 400, new ValueCallback() {
						@Override
						public void execute(final String value) {
							if (!readonly) {
								user.setDocsGrid(value);
								changedHandler.onChanged(null);
								docsGrid.setValue(value != null && !value.isEmpty() ? I18N.message(CUSTOMIZED)
										: I18N.message(NOTCUSTOMIZED));
							}
						}
					});
			event.cancel();
		});
		docsGrid.setIcons(editDocsLayout, clearIcon);
		return docsGrid;
	}

	private StaticTextItem prepareHitsGridLayoutItem(boolean readonly, PickerIcon clearIcon) {
		StaticTextItem hitsGrid = ItemFactory.newStaticTextItem(HITSGRID, HITSGRIDLAYOUT,
				user.getHitsGrid() != null && !user.getHitsGrid().isEmpty() ? I18N.message(CUSTOMIZED)
						: I18N.message(NOTCUSTOMIZED));
		FormItemIcon editHitsLayout = new FormItemIcon();
		editHitsLayout.setSrc("[SKIN]/paste.gif");
		editHitsLayout.setPrompt(I18N.message("editlayout"));
		editHitsLayout.setWidth(12);
		editHitsLayout.setHeight(12);
		editHitsLayout.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem(HITSGRIDLAYOUT, null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message(HITSGRIDLAYOUT), I18N.message(HITSGRIDLAYOUT),
					user.getHitsGrid() != null ? user.getHitsGrid() : "", textArea, 400, (final String value) -> {
						if (!readonly) {
							user.setHitsGrid(value);
							changedHandler.onChanged(null);
							hitsGrid.setValue(value != null && !value.isEmpty() ? I18N.message(CUSTOMIZED)
									: I18N.message(NOTCUSTOMIZED));
						}
					});
			event.cancel();
		});
		hitsGrid.setIcons(editHitsLayout, clearIcon);
		return hitsGrid;
	}

	private PickerIcon prepareClearPickerIcon(boolean readonly) {
		PickerIcon clearIcon = new PickerIcon(PickerIcon.CLEAR, (FormItemIconClickEvent event) -> {
			event.getItem().setValue(I18N.message(NOTCUSTOMIZED));
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

		if (vm.getValueAsString(DOCSGRID) == null || vm.getValueAsString(DOCSGRID).isEmpty()
				|| I18N.message(NOTCUSTOMIZED).equals(vm.getValueAsString(DOCSGRID)))
			user.setDocsGrid(null);
		if (vm.getValueAsString(HITSGRID) == null || vm.getValueAsString(HITSGRID).isEmpty()
				|| I18N.message(NOTCUSTOMIZED).equals(vm.getValueAsString(HITSGRID)))
			user.setHitsGrid(null);

		if (vm.getValueAsString(DATEFORMAT) == null || vm.getValueAsString(DATEFORMAT).isEmpty())
			user.setDateFormat(null);
		else
			user.setDateFormat(vm.getValueAsString(DATEFORMAT));

		if (vm.getValueAsString(DATEFORMATSHORT) == null || vm.getValueAsString(DATEFORMATSHORT).isEmpty())
			user.setDateFormatShort(null);
		else
			user.setDateFormatShort(vm.getValueAsString(DATEFORMATSHORT));

		if (vm.getValueAsString(DATEFORMATLONG) == null || vm.getValueAsString(DATEFORMATLONG).isEmpty())
			user.setDateFormatLong(null);
		else
			user.setDateFormatLong(vm.getValueAsString(DATEFORMATLONG));
	}
}