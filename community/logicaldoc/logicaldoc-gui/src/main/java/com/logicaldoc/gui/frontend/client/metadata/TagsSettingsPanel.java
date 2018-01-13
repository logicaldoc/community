package com.logicaldoc.gui.frontend.client.metadata;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows some general settings for the tags management.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.4
 */
public class TagsSettingsPanel extends VLayout {

	private ValuesManager vm = new ValuesManager();

	public TagsSettingsPanel(GUIParameter[] settings) {
		setMembersMargin(5);
		setMargin(5);

		DynamicForm parametersForm = new DynamicForm();
		parametersForm.setValuesManager(vm);
		parametersForm.setTitleOrientation(TitleOrientation.LEFT);
		parametersForm.setNumCols(2);
		parametersForm.setColWidths(1, "*");
		parametersForm.setPadding(5);

		SelectItem mode = ItemFactory.newTagInputMode("mode", "inputmode");

		SpinnerItem maxsize = ItemFactory.newSpinnerItem("maxsize", I18N.message("maxsize"), (Long) null);
		maxsize.setRequired(true);

		SpinnerItem minsize = ItemFactory.newSpinnerItem("minsize", I18N.message("minsize"), (Long) null);
		minsize.setRequired(true);

		SpinnerItem cloudElements = ItemFactory.newSpinnerItem("cloudElements", I18N.message("tagcloudelements"),
				(Long) null);
		cloudElements.setRequired(true);
		cloudElements.setWrapTitle(false);

		TextItem vocabulary = ItemFactory.newTextItem("vocabulary", I18N.message("vocabulary"), null);
		vocabulary.setRequired(true);
		vocabulary.setWidth(300);

		parametersForm.setItems(mode, maxsize, minsize, cloudElements, vocabulary);
		addMember(parametersForm);

		for (GUIParameter p : settings) {
			if (p.getName().endsWith("tag.mode"))
				mode.setValue(p.getValue());
			if (p.getName().endsWith("tag.maxsize"))
				maxsize.setValue(p.getValue());
			if (p.getName().endsWith("tag.minsize"))
				minsize.setValue(p.getValue());
			if (p.getName().endsWith("tag.vocabulary"))
				vocabulary.setValue(p.getValue());
			if (p.getName().endsWith("tagcloud.maxtags"))
				cloudElements.setValue(p.getValue());
		}

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				final Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					List<GUIParameter> params = new ArrayList<GUIParameter>();

					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.mode", values.get("mode")
							.toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.maxsize", values.get("maxsize")
							.toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.minsize", values.get("minsize")
							.toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.vocabulary", values.get(
							"vocabulary").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tagcloud.maxtags", values.get(
							"cloudElements").toString()));

					SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]),
							new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Session.get()
											.getInfo()
											.setConfig(Session.get().getTenantName() + ".tag.mode",
													values.get("mode").toString());
									Session.get()
											.getInfo()
											.setConfig(Session.get().getTenantName() + ".tag.maxsize",
													values.get("maxsize").toString());

									Session.get()
											.getInfo()
											.setConfig(Session.get().getTenantName() + ".tag.minsize",
													values.get("minsize").toString());

									Session.get()
											.getInfo()
											.setConfig(Session.get().getTenantName() + ".tagcloud.maxtags",
													values.get("cloudElements").toString());

									Log.info(I18N.message("settingssaved"), null);
								}
							});
				}
			}
		});

		addMember(save);
	}
}