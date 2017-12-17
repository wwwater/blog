#!/bin/bash


function deploy_frontend() {
    echo -e "Deploying frontend\n"
    cd frontend
    elm test
    test_passed=`elm test | grep -c "TEST RUN PASSED"`
    if [[ $test_passed == "1" ]]
    then
        npm run build
        sed -i s'|http://localhost:8081|https://cyclinginthewind.com/api|' dist/index.js
        scp -r dist 46.101.142.224:~/cyclinginthewind/frontend/
    else
        echo "Tests failed. Not deploying anything! :)"
    fi
    cd ..
}

function deploy_backend() {
    echo -e "Deploying backend\n"
    cd backend
    stack test #--coverage --ghc-options "-fforce-recomp"
    test_passed=`stack test | grep -c "0 failures"`
    if [[ $test_passed == "1" ]]
    then
        stack image container
        echo "Saving docker image as a tar. Could take a while.."
        docker save cyclinginthewind-backend | gzip > cyclinginthewind-backend.tar.gz
        scp cyclinginthewind-backend.tar.gz 46.101.142.224:~/cyclinginthewind/
        ssh 46.101.142.224 "
            cd cyclinginthewind;
            gzip -df cyclinginthewind-backend.tar.gz;
            echo 'Loading docker image from tar file. Could take a while..';
            docker load -i cyclinginthewind-backend.tar;
            ./restart_backend.sh;"
    else
        echo "Tests failed. Not deploying anything! =)"
    fi
    cd ..
}

function backup_database() {
    echo -e "Backup database from server on dropbox\n"
    today=`date +"%Y-%m-%d"`
    scp 46.101.142.224:~/cyclinginthewind/db/blog.db ~/Dropbox/backup/digital_ocean/blog_"$today".db
}

if [[ $# -eq 0 ]] 
then
    echo "An argument --frontend or --backend or --backup-db is needed."
    exit
fi

while [[ $# > 0 ]]
do
    case "${1}" in
      -F|--frontend)
      deploy_frontend
      shift
      ;;
      -B|--backend)
      deploy_backend
      shift
      ;;
      -DB|--backup-db)
      backup_database
      shift
      ;;
      *)
      echo "${1} is not a valid flag. Only --frontend or --backend or
        --backup-db are acceptable."
      shift
      ;;
    esac
done
