#include <sys/module.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/types.h>
#include <sys/pserialize.h>

static int lab3_modcmd(modcmd_t cmd, void *arg) {
    struct proc *p;
    int error = 0;

    switch (cmd) {
        case MODULE_CMD_INIT:
            printf("lab3: loading module\n");

            int s = pserialize_read_enter();
    	    PROCLIST_FOREACH(p, &allproc) {
        		printf(" %d: \t%s       \t(Parent PID: %d)\n",
               		p->p_pid, p->p_comm, p->p_ppid);
    	    }
    	    pserialize_read_exit(s);

            break;

        case MODULE_CMD_FINI:
            printf("lab3: unloading module\n");
            break;

        default:
            error = ENOTTY;
            break;
    }

    return error;
}

MODULE(MODULE_CLASS_DRIVER, lab3, NULL);
