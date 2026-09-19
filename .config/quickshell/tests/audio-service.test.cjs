const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const test = require("node:test");
function handlers(file) { return fs.readFileSync(`${__dirname}/../${file}`,"utf8").match(/^    function \w+\([^\n]*\) \{[\s\S]*?^    \}/gm).join("\n"); }
function fixture() {
    const node={ready:true,isStream:false,isSink:true,audio:{volume:.5,muted:true}};
    const c={ready:true,sink:node,audio:node.audio,volume:.5,open:false,anchorWindow:null,anchorItem:null,
        streamCount:0,groupCount:0,Lock:{locked:false},Pipewire:{nodes:{values:[node]}},
        Brightness:{close(){}}};
    c.root=c;vm.createContext(c);vm.runInContext(handlers("Services/Audio.qml"),c);
    return {c,node};
}
test("mute preserves volume; explicit writes unmute, clamp and reject invalid values",()=>{
    const {c,node}=fixture();
    c.toggleMute();assert.equal(node.audio.muted,false);assert.equal(node.audio.volume,.5);
    c.setVolume(node,2);assert.equal(node.audio.volume,1);
    c.setVolume(node,-1);assert.equal(node.audio.volume,0);
    node.audio.muted=true;c.setVolume(node,.4);assert.equal(node.audio.muted,false);
    assert.equal(c.setVolume(node,NaN),false);assert.equal(c.setVolume(node,Infinity),false);
    assert.equal(node.audio.volume,.4);
});
test("destroyed or unready objects cannot receive stale writes",()=>{
    const {c,node}=fixture();node.ready=false;assert.equal(c.setVolume(node,.2),false);
    node.ready=true;c.Pipewire.nodes.values=[];
    assert.equal(c.setVolume(node,.2),false);assert.equal(c.toggleNodeMute(node),false);
    assert.equal(c.setVolume(null,.2),false);assert.equal(node.audio.volume,.5);
});
test("device direction and actual object membership guard preference writes",()=>{
    const {c,node}=fixture();
    assert.equal(c.selectDevice(node,true),false);
    assert.equal(c.selectDevice(node,false),true);assert.equal(c.Pipewire.preferredDefaultAudioSink,node);
    node.isStream=true;assert.equal(c.selectDevice(node,false),false);
});
test("popup clears anchors/counts and refuses to open while locked",()=>{
    const {c}=fixture();const anchor={};
    assert.equal(c.toggle(anchor,anchor),true);assert.equal(c.anchorItem,anchor);
    c.streamCount=4;c.close();assert.equal(c.anchorItem,null);assert.equal(c.streamCount,0);
    c.Lock.locked=true;assert.equal(c.toggle(anchor,anchor),false);assert.equal(c.open,false);
});
test("shared OSD path suppresses both volume entry points but permits brightness",()=>{
    const c={armed:true,open:false,kind:"volume",Audio:{open:true},Brightness:{present:true,open:false},
        hide:{starts:0,restart(){this.starts++}}};
    c.root=c;vm.createContext(c);vm.runInContext(handlers("Services/Osd.qml"),c);
    c.show("volume");c.showVolume();assert.equal(c.open,false);assert.equal(c.hide.starts,0);
    c.showBrightness();assert.equal(c.kind,"brightness");assert.equal(c.hide.starts,1);
    c.Audio.open=false;c.showVolume();assert.equal(c.kind,"volume");assert.equal(c.hide.starts,2);
});

test("opening mixer dismisses an existing volume OSD without hiding brightness",()=>{
    const c={open:true,kind:"volume",Audio:{open:true},hide:{stops:0,stop(){this.stops++}}};
    c.root=c;vm.createContext(c);
    const source=fs.readFileSync(`${__dirname}/../Services/Osd.qml`,"utf8");
    vm.runInContext(source.match(/        function onOpenChanged\(\) \{[\s\S]*?^        \}/m)[0],c);
    c.onOpenChanged();assert.equal(c.open,false);assert.equal(c.hide.stops,1);
    c.kind="brightness";c.open=true;c.onOpenChanged();assert.equal(c.open,true);assert.equal(c.hide.stops,1);
});

test("brightness OSD is suppressed while the brightness popup is open",()=>{
    const c={armed:true,open:false,kind:"volume",Audio:{open:false},Brightness:{present:true,open:true},
        hide:{starts:0,restart(){this.starts++}}};
    c.root=c;vm.createContext(c);vm.runInContext(handlers("Services/Osd.qml"),c);
    c.show("brightness");c.showBrightness();assert.equal(c.open,false);assert.equal(c.hide.starts,0);
    c.Brightness.open=false;c.showBrightness();assert.equal(c.kind,"brightness");assert.equal(c.hide.starts,1);
});
