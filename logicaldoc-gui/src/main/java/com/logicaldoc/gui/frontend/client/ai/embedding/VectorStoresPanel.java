package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A panel to handle the supported vector stores
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class VectorStoresPanel extends VLayout {

	private ValuesManager vm = new ValuesManager();

	public VectorStoresPanel() {
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		AIService.Instance.get().loadVectorStore(new DefaultAsyncCallback<List<GUIParameter>>() {

			@Override
			protected void handleSuccess(List<GUIParameter> settings) {
				initGUI(settings);
			}
		});
	}

	void initGUI(List<GUIParameter> settings) {
		// Url
		TextItem url = ItemFactory.newTextItem("url", Util.getValue("url", settings));
		url.setWidth(400);
		url.setRequired(true);
		url.setHint("jdbc:mariadb://host:port/database");
		url.setShowHintInField(true);

		FormItemIcon composer = new FormItemIcon();
		composer.setPrompt(I18N.message("mariadburlcomposer"));
		composer.setSrc("[SKIN]/icons/wand-magic-sparkles.png");
		composer.addFormItemClickHandler(click -> new MariaDBComposer(url).show());
		url.setIcons(composer);

		// Username
		TextItem username = ItemFactory.newTextItem("username", Util.getValue("username", settings));
		username.setRequired(false);

		// Password
		PasswordItem password = ItemFactory.newPasswordItem("password", "Password",
				Util.getValue("password", settings));
		password.setRequired(false);

		// Test button
		ButtonItem testButton = new ButtonItem(I18N.message("testconnection"));
		testButton.addClickHandler(event -> {
			if (Boolean.TRUE.equals(vm.validate())) {
				List<GUIParameter> params = collectSettings();

				AIService.Instance.get().testVectorStore(params, new DefaultAsyncCallback<Boolean>() {
					@Override
					protected void handleSuccess(Boolean result) {
						if (Boolean.TRUE.equals(result))
							SC.say(I18N.message("connectionestablished"));
						else
							SC.warn(I18N.message("connectionfailed"));
					}

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}
				});
			}
		});

		DynamicForm mariadbForm = new DynamicForm();
		mariadbForm.setValuesManager(vm);
		mariadbForm.setTitleOrientation(TitleOrientation.LEFT);
		mariadbForm.setNumCols(2);
		mariadbForm.setPadding(5);
		mariadbForm.setIsGroup(true);
		mariadbForm.setGroupTitle("MariaDB");
		mariadbForm.setFields(url, username, password, testButton);

		addMember(mariadbForm);

		// Save Button
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (Boolean.TRUE.equals(vm.validate())) {
					AIService.Instance.get().saveVectorStore(collectSettings(), new DefaultAsyncCallback<Void>() {
						@Override
						protected void handleSuccess(Void result) {
							GuiLog.info(I18N.message("settingssaved"), null);
						}
					});
				}
			}
		});
		addMember(save);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	private List<GUIParameter> collectSettings() {
		List<GUIParameter> params = new ArrayList<>();

		params.add(new GUIParameter("url", vm.getValueAsString("url")));
		params.add(new GUIParameter("username", vm.getValueAsString("username")));
		params.add(new GUIParameter("password", vm.getValueAsString("password")));

		return params;
	}
}