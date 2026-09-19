#include <QQuickItem>
#include <QtQuickTest/quicktest.h>

// No children are launched. Model Quickshell v0.3.1's writable running getter:
// termination is asynchronous, failed start has no exited signal, and a queued
// restart begins after exited/runningChanged. See src/io/process.cpp upstream.
class ProcessFixture : public QQuickItem {
    Q_OBJECT
    Q_PROPERTY(bool running READ running WRITE setRunning NOTIFY runningChanged)
    Q_PROPERTY(QVariant command MEMBER command)
    Q_PROPERTY(QObject* stdout MEMBER output)
    Q_PROPERTY(int starts READ starts NOTIFY startsChanged)
public:
    bool running() const { return alive; }
    int starts() const { return attempts; }
    void setRunning(bool on) {
        queued = on;
        if (on) startQueued();
    }
    Q_INVOKABLE void started() { emit runningChanged(); }
    Q_INVOKABLE void failStart() {
        alive = false;
        emit runningChanged();
    }
    Q_INVOKABLE void finish() {
        alive = false;
        emit exited(0, 0);
        emit runningChanged();
        startQueued();
    }
signals:
    void runningChanged();
    void startsChanged();
    void exited(int exitCode, int exitStatus);
private:
    void startQueued() {
        if (alive || !queued) return;
        queued = false;
        alive = true;
        ++attempts;
        emit startsChanged();
    }
    bool alive = false;
    bool queued = false;
    int attempts = 0;
    QVariant command;
    QObject* output = nullptr;
};

int main(int argc, char** argv) {
    qmlRegisterType<ProcessFixture>("Quickshell.Io", 1, 0, "Process");
    return quick_test_main(argc, argv, "process-lifecycle", nullptr);
}

#include "process-lifecycle-fixture.moc"
