// TeXworksScript
// Title: Sweave Preview
// Description: Looks for PDF filename to preview
// Author: Duncan Murdoch
// Version: 0.1
// Date: 2013-12-01
// Script-Type: hook
// Hook: AfterTypeset

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
 
outputRE = new RegExp("^Output written on ([^(]*)[.](dvi|pdf) \\(");

function getBasePath(path) {
    var i = Math.max(path.lastIndexOf('/'), 
                     path.lastIndexOf('\\'));
    return (i == -1) ? path : path.slice(0, i+1);
}

// get the text from the standard console output
txt = TW.target.consoleOutput;
lines = txt.split('\n');

filename = "Not found";
done = 0;

for (i = lines.length - 1; i >= 0; --i) {
    line = lines[i];
        
    matched = outputRE.exec(line);
    if (matched) {
        filename = getBasePath(TW.target.rootFileName) + 
                   matched[1] + ".pdf";
        windows = TW.app.getOpenWindows();

        for (j = 0; j < windows.length; j++) {
            win = windows[j];
            if (win.fileName == filename) {
                win.reload();
                done = 1;
            }
        }
        if (!done)
            TW.app.openFileFromScript(filename, TW);
        break;
    }
}

if (done) 
  TW.result = "Attempted to reload " + filename;
else
  TW.result = "Attempted to load " + filename;
