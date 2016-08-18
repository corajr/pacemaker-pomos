# pacemaker-pomos
Turn a Pacemaker schedule into Pomodoros starting at a certain time each day.

## Usage

Download your writing schedule from [Pacemaker](https://pacemaker.press), then:

```
stack exec pacemaker-pomos < ~/Downloads/PacemakerWritingSchedule.ics > pacemaker-pomos.ics
```

Start time is given as an hour in UTC.

To start at, e.g. 11 AM EST, use:

```
stack exec pacemaker-pomos -- 15
```

To set the rate of words per Pomodoro (default 125 words per 25 minutes), give
two arguments:

```
stack exec pacemaker-pomos -- 15 200
```

