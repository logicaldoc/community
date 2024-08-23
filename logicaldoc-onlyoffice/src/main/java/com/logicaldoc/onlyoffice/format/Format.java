/**
 *
 * (c) Copyright LogicalDOC SRL 2024
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.logicaldoc.onlyoffice.format;

import java.util.List;

import com.logicaldoc.onlyoffice.entities.FileType;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

public final class Format {

    private String name;
	private FileType type;
    private List<String>  actions;
    private List<String> convert;
    private List<String> mime;    
    
    public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public FileType getType() {
		return type;
	}
	public void setType(FileType type) {
		this.type = type;
	}
	public List<String> getActions() {
		return actions;
	}
	public void setActions(List<String> actions) {
		this.actions = actions;
	}
	public List<String> getConvert() {
		return convert;
	}
	public void setConvert(List<String> convert) {
		this.convert = convert;
	}
	public List<String> getMime() {
		return mime;
	}
	public void setMime(List<String> mime) {
		this.mime = mime;
	}

}
